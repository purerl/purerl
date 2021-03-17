{-# LANGUAGE GADTs #-}

-- |
-- Functions for replacing fully applied type synonyms
--
module Language.PureScript.Erl.Synonyms
  ( SynonymMap
  , KindMap
  , replaceRecordRowTypeSynonymsM
  , replaceAllTypeSynonyms'
  ) where

import           Prelude.Compat

import           Control.Monad.Error.Class (MonadError(..))
import           Data.Maybe (fromMaybe)
import qualified Data.Map as M
import           Data.Text (Text)
import           Language.PureScript.Environment
import           Language.PureScript.Erl.Errors
import           Language.PureScript.Erl.Errors.Types
import           Language.PureScript.Names
import           Language.PureScript.Types
import           Language.PureScript.AST

-- | Type synonym information (arguments with kinds, aliased type), indexed by name
type SynonymMap = M.Map (Qualified (ProperName 'TypeName)) ([(Text, Maybe SourceType)], SourceType)

type KindMap = M.Map (Qualified (ProperName 'TypeName)) (SourceType, TypeKind)

replaceRecordRowTypeSynonyms'
  :: SynonymMap
  -> KindMap
  -> SourceType
  -> Either MultipleErrors SourceType
replaceRecordRowTypeSynonyms' syns kinds = everywhereOnTypesTopDownM try
  where
  try :: SourceType -> Either MultipleErrors SourceType
  try t = fromMaybe t <$> go t

  go :: SourceType -> Either MultipleErrors (Maybe SourceType)
  go t@(TypeApp _ tr _) | tr == tyRecord 
    = Just <$> replaceAllTypeSynonyms' syns kinds t                   
  go _ = return Nothing

replaceAllTypeSynonyms'
  :: SynonymMap
  -> KindMap
  -> SourceType
  -> Either MultipleErrors SourceType
replaceAllTypeSynonyms' syns kinds = everywhereOnTypesTopDownM try
  where
  try :: SourceType -> Either MultipleErrors SourceType
  try t = fromMaybe t <$> go (fst $ getAnnForType t) 0 [] [] t

  go :: SourceSpan -> Int -> [SourceType] -> [SourceType] -> SourceType -> Either MultipleErrors (Maybe SourceType)
  go _ c kargs args (TypeConstructor _ ctor)
    | Just (synArgs, body) <- M.lookup ctor syns
    , c == length synArgs
    , kindArgs <- lookupKindArgs ctor
    , length kargs == length kindArgs
    = let repl = replaceAllTypeVars (zip (map fst synArgs) args <> zip kindArgs kargs) body
      in Just <$> try repl
    | Just (synArgs, _) <- M.lookup ctor syns
    , length synArgs > c
    = throwError . errorMessage $ InternalError "Partially applied type synonym"
  go ss c kargs args (TypeApp _ f arg) = go ss (c + 1) kargs (arg : args) f
  go ss c kargs args (KindApp _ f arg) = go ss c (arg : kargs) args f
  go _ _ _ _ _ = return Nothing

  lookupKindArgs :: Qualified (ProperName 'TypeName) -> [Text]
  lookupKindArgs ctor = fromMaybe [] $ fmap (fmap (fst . snd) . fst) . completeBinderList . fst =<< M.lookup ctor kinds


-- | Replace fully applied type synonyms by explicitly providing a 'SynonymMap'.
replaceRecordRowTypeSynonymsM
  :: MonadError MultipleErrors m
  => SynonymMap
  -> KindMap
  -> SourceType
  -> m SourceType
replaceRecordRowTypeSynonymsM syns kinds = either throwError pure . replaceRecordRowTypeSynonyms' syns kinds
