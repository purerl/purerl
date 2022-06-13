module Language.PureScript.Erl.CodeGen.CheckedWrapper where

import Control.Monad.Supply.Class (MonadSupply)
import qualified Data.Text as T
import Language.PureScript (ModuleName)
import Language.PureScript.Erl.CodeGen.AST
import Language.PureScript.Erl.CodeGen.Common (ModuleType (PureScriptModule), atomModuleName, freshNameErl)
import Language.PureScript.PSString (mkString)
import Prelude

data Path
  = PathRecord Path Atom
  | PathArray Path -- TODO[fh]: would be nice if we also fetched/stored the array index
  | PathRoot T.Text Int T.Text
  deriving (Show)

typecheckWrapper :: forall m. (Monad m, MonadSupply m) => ModuleName -> Erl -> m [Erl]
typecheckWrapper mn =
  \case
    EFunctionDef (Just (TFun [] _)) _ _ [] _ ->
      -- TODO[fh]: is this correct? feels odd that both the list of args is empty and the body is a nullary fn, I expected one or the other but not both
      pure []
    EFunctionDef (Just t) sourceSpan fnName@(Atom _ fnNameRaw) argNames _ ->
      do
        let pathToPSString =
              \case
                PathRecord p (Atom _ field) -> pathToPSString p <> "." <> mkString field
                PathRecord p (AtomPS _ field) -> pathToPSString p <> "." <> field
                PathArray p -> pathToPSString p <> "[?]"
                PathRoot s idx var -> mkString s <> "->" <> mkString (T.pack $ show idx) <> "(" <> mkString var <> ")"

            typeError path thing =
              ( EBinder (EVar "_"),
                EApp RegularApp
                  (EAtomLiteral (Atom (Just "erlang") "error"))
                  [EStringLiteral $ "purerl runtime ffi type error: " <> pathToPSString path <> " " <> thing]
              )

            simpleCheck checkVar binderVar check =
              let binderVar' =
                    if binderVar == checkVar
                      then EVar "_"
                      else binderVar
               in (EGuardedBinder binderVar' (Guard (EApp RegularApp (EAtomLiteral (Atom (Just "erlang") check)) [checkVar])), EAtomLiteral (Atom Nothing "typecheck"))

            -- try to reuse bound names if possible
            maybeFreshVar x@(EVar _) = pure x
            maybeFreshVar _ = EVar <$> freshNameErl

            zipArgTypes [] _ = []
            zipArgTypes (argName : rest) (TFun (argT : restT) rhs) = (argName, argT) : zipArgTypes rest (TFun restT rhs)
            zipArgTypes _ _ = error "unexpected arg to zipArgTypes"

            typeArg :: Path -> (Erl, EType) -> m (Maybe [Erl])
            typeArg path (argName, argT) =
              -- fmap (EComment (T.pack $ show argName <> " :: " <> show argT) :) <$>
              case argT of
                TInteger -> do
                  bindName <- maybeFreshVar argName
                  pure $
                    Just
                      [ ECaseOf
                          argName
                          [ simpleCheck argName bindName "is_integer",
                            typeError path "is not an integer"
                          ]
                      ]
                TFloat -> do
                  bindName <- maybeFreshVar argName
                  pure $
                    Just
                      [ ECaseOf
                          argName
                          [ simpleCheck argName bindName "is_float",
                            typeError path "is not a float"
                          ]
                      ]
                TAlias (Atom Nothing "boolean") [] -> do
                  bindName <- maybeFreshVar argName
                  pure $
                    Just
                      [ ECaseOf
                          argName
                          [ simpleCheck argName bindName "is_boolean",
                            typeError path "is not true or false"
                          ]
                      ]
                TAlias (Atom Nothing "binary") [] -> do
                  bindName <- maybeFreshVar argName
                  pure $
                    Just
                      [ ECaseOf
                          argName
                          [ simpleCheck argName bindName "is_binary",
                            typeError path "is not an utf-8 encoded binary"
                          ]
                      ]
                TRemote "array" "array" [innerType] -> do
                  bindName <- maybeFreshVar argName
                  innerName <- EVar <$> freshNameErl
                  mInnerTypeCheck <- typeArg (PathArray path) (innerName, innerType)
                  case mInnerTypeCheck of
                    Nothing -> pure Nothing
                    Just innerTypeCheck ->
                      pure $
                        Just
                          [ ECaseOf
                              (EApp RegularApp (EAtomLiteral (Atom (Just "array") "is_array")) [bindName])
                              [ ( EBinder (EAtomLiteral (Atom Nothing "true")),
                                  EApp RegularApp
                                    (EAtomLiteral (Atom (Just "array") "map"))
                                    [ EFunFull
                                        Nothing
                                        [ ( EFunBinder [innerName] Nothing,
                                            EBlock innerTypeCheck
                                          )
                                        ],
                                      bindName
                                    ]
                                ),
                                typeError path "failed is_array check; it's not an array"
                              ]
                          ]
                TMap (Just pairs) ->
                  let unwrapMapKeys :: [(EType, EType)] -> Maybe [(Atom, EType)]
                      unwrapMapKeys [] = Just []
                      unwrapMapKeys ((TAtom (Just k), v) : rest) =
                        unwrapMapKeys rest <> Just [(k, v)]
                      unwrapMapKeys _ = Nothing
                   in case unwrapMapKeys pairs of
                        Nothing -> pure Nothing
                        Just unwrapped -> do
                          trios <-
                            traverse
                              ( \(a, v) -> do
                                  n <- freshNameErl
                                  pure (a, EVar n, v)
                              )
                              unwrapped
                          let aePairs = map (\(a, e, _) -> (a, e)) trios
                          mtypechecks :: Maybe [Erl] <- traverse (fmap EBlock) <$> traverse (\(a, e, v) -> typeArg (PathRecord path a) (e, v)) trios

                          case mtypechecks of
                            Nothing -> pure Nothing
                            Just (typechecks :: [Erl]) ->
                              pure $
                                Just
                                  [ ECaseOf
                                      argName
                                      [ ( EGuardedBinder
                                            (EMapPattern aePairs)
                                            ( Guard
                                                ( EBinary
                                                    EqualTo
                                                    (EApp RegularApp (EAtomLiteral (Atom (Just "erlang") "map_size")) [argName])
                                                    (ENumericLiteral (Left (fromIntegral (length aePairs))))
                                                )
                                            ),
                                          EBlock typechecks
                                        ),
                                        ( EBinder (EMapPattern aePairs),
                                          snd (typeError path "failed map_size check; there's too many fields in this record")
                                        ),
                                        typeError path "is missing at least one field"
                                      ]
                                  ]
                _ ->
                  -- default to mark function as unsafe
                  pure Nothing

        mtargs <- traverse (fmap EBlock) <$> traverse (\(idx, (argName, argType)) -> typeArg (PathRoot fnNameRaw idx argName) (EVar argName, argType)) (zip [0 ..] $ zipArgTypes argNames t)
        case mtargs of
          Nothing -> pure []
          Just targs ->
            pure $
              [ EFunctionDef (Just t) sourceSpan fnName argNames $
                  EBlock $
                    targs
                      <> [EApp RegularApp (EAtomLiteral $ Atom (Just $ atomModuleName mn PureScriptModule) fnNameRaw) (map EVar argNames)]
              ]
    _ ->
      pure []