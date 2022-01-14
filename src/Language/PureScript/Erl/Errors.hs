{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.PureScript.Erl.Errors
  ( module Language.PureScript.Erl.Errors
  ) where

import           Prelude.Compat
import           Protolude (ordNub)

import           Control.Arrow ((&&&))
import           Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.State.Lazy ( evalState, get )
import Control.Monad.Writer
    ( unless,
      forM,
      Last(Last, getLast),
      censor,
      MonadWriter(tell, listen) )
import           Control.Exception (displayException)
import           Data.Char (isSpace)
import           Data.Either (partitionEithers)
import           Data.Foldable (fold)
import           Data.List (nubBy, partition, dropWhileEnd, sort, sortOn)
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text (Text)
import           Language.PureScript.AST (ErrorMessageHint(..), HintCategory(..), DeclarationRef(..), ImportDeclarationType(..), Expr(..))
import           Language.PureScript.AST.SourcePos
import qualified Language.PureScript.Constants.Prim as C
import Language.PureScript.Crash ( internalError )
import Language.PureScript.Environment ( primSubName )
import           Language.PureScript.Label (Label(..))
import Language.PureScript.Names
    ( ModuleName,
      runIdent,
      runModuleName,
      showIdent,
      showQualified,
      ProperName(runProperName) )
import Language.PureScript.Pretty
    ( prettyPrintLabel,
      typeAsBox,
      typeAtomAsBox,
      typeDiffAsBox,
      prettyPrintValue )
import           Language.PureScript.Errors (prettyPrintRef)
import           Language.PureScript.Pretty.Common (endWith)
import           Language.PureScript.PSString (decodeStringWithReplacement)
import Language.PureScript.Types
    ( Type(TypeConstructor, RCons, TypeLevelString, TypeApp),
      eqType,
      rowFromList,
      rowToList,
      Constraint(Constraint),
      RowListItem(RowListItem) )
import Language.PureScript.Erl.Errors.Types
    ( ErrorMessage(..), SimpleErrorMessage(..) )
import qualified System.Console.ANSI as ANSI
import qualified Text.Parsec as P
import qualified Text.Parsec.Error as PE
import           Text.Parsec.Error (Message(..))
import qualified Text.PrettyPrint.Boxes as Box

newtype ErrorSuggestion = ErrorSuggestion Text

-- | Get the source span for an error
errorSpan :: ErrorMessage -> Maybe (NEL.NonEmpty SourceSpan)
errorSpan = findHint matchSpan
  where
  matchSpan (PositionedError ss) = Just ss
  matchSpan _ = Nothing

-- | Get the module name for an error
errorModule :: ErrorMessage -> Maybe ModuleName
errorModule = findHint matchModule
  where
  matchModule (ErrorInModule mn) = Just mn
  matchModule _ = Nothing

findHint :: (ErrorMessageHint -> Maybe a) -> ErrorMessage -> Maybe a
findHint f (ErrorMessage hints _) = getLast . foldMap (Last . f) $ hints

-- | Remove the module name and span hints from an error
stripModuleAndSpan :: ErrorMessage -> ErrorMessage
stripModuleAndSpan (ErrorMessage hints e) = ErrorMessage (filter (not . shouldStrip) hints) e
  where
  shouldStrip (ErrorInModule _) = True
  shouldStrip (PositionedError _) = True
  shouldStrip _ = False

-- | Get the error code for a particular error type
errorCode :: ErrorMessage -> Text
errorCode em = case unwrapErrorMessage em of
  MissingFFIModule{} -> "MissingFFIModule"
  UnnecessaryFFIModule{} -> "UnnecessaryFFIModule"
  MissingFFIImplementations{} -> "MissingFFIImplementations"
  UnusedFFIImplementations{} -> "UnusedFFIImplementations"
  InvalidFFIArity{} -> "InvalidFFIArity"
  FileIOError{} -> "FileIOError"
  InternalError{} -> "InternalError"


-- | A stack trace for an error
newtype MultipleErrors = MultipleErrors
  { runMultipleErrors :: [ErrorMessage]
  } deriving (Show, Semigroup, Monoid)

-- | Check whether a collection of errors is empty or not.
nonEmpty :: MultipleErrors -> Bool
nonEmpty = not . null . runMultipleErrors

-- | Create an error set from a single simple error message
errorMessage :: SimpleErrorMessage -> MultipleErrors
errorMessage err = MultipleErrors [ErrorMessage [] err]

-- | Create an error set from a single simple error message and source annotation
errorMessage' :: SourceSpan -> SimpleErrorMessage -> MultipleErrors
errorMessage' ss err = MultipleErrors [ErrorMessage [positionedError ss] err]

-- | Create an error set from a single simple error message and source annotations
errorMessage'' :: NEL.NonEmpty SourceSpan -> SimpleErrorMessage -> MultipleErrors
errorMessage'' sss err = MultipleErrors [ErrorMessage [PositionedError sss] err]

-- | Create an error from multiple (possibly empty) source spans, reversed sorted.
errorMessage''' :: [SourceSpan] -> SimpleErrorMessage -> MultipleErrors
errorMessage''' sss err =
  maybe (errorMessage err) (`errorMessage''` err)
    . NEL.nonEmpty
    . reverse
    . sort
    $ filter (/= NullSourceSpan) sss

-- | Create an error set from a single error message
singleError :: ErrorMessage -> MultipleErrors
singleError = MultipleErrors . pure

-- | Lift a function on ErrorMessage to a function on MultipleErrors
onErrorMessages :: (ErrorMessage -> ErrorMessage) -> MultipleErrors -> MultipleErrors
onErrorMessages f = MultipleErrors . map f . runMultipleErrors

-- | Add a hint to an error message
addHint :: ErrorMessageHint -> MultipleErrors -> MultipleErrors
addHint hint = addHints [hint]

-- | Add hints to an error message
addHints :: [ErrorMessageHint] -> MultipleErrors -> MultipleErrors
addHints hints = onErrorMessages $ \(ErrorMessage hints' se) -> ErrorMessage (hints ++ hints') se

-- | A map from rigid type variable name/unknown variable pairs to new variables.
data TypeMap = TypeMap
  { umSkolemMap   :: M.Map Int (String, Int, Maybe SourceSpan)
  -- ^ a map from skolems to their new names, including source and naming info
  , umUnknownMap  :: M.Map Int Int
  -- ^ a map from unification variables to their new names
  , umNextIndex   :: Int
  -- ^ unknowns and skolems share a source of names during renaming, to
  -- avoid overlaps in error messages. This is the next label for either case.
  } deriving Show

defaultUnknownMap :: TypeMap
defaultUnknownMap = TypeMap M.empty M.empty 0

-- | How critical the issue is
data Level = Error | Warning deriving Show

-- | Extract nested error messages from wrapper errors
unwrapErrorMessage :: ErrorMessage -> SimpleErrorMessage
unwrapErrorMessage (ErrorMessage _ se) = se

errorDocUri :: ErrorMessage -> Text
errorDocUri e = "https://github.com/purescript/documentation/blob/master/errors/" <> errorCode e <> ".md"

ansiColor :: (ANSI.ColorIntensity, ANSI.Color) -> String
ansiColor (intesity, color) =
   ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground intesity color]

ansiColorReset :: String
ansiColorReset =
   ANSI.setSGRCode [ANSI.Reset]

colorCode :: Maybe (ANSI.ColorIntensity, ANSI.Color) -> Text -> Text
colorCode codeColor code = case codeColor of
  Nothing -> code
  Just cc -> T.pack (ansiColor cc) <> code <> T.pack ansiColorReset

colorCodeBox :: Maybe (ANSI.ColorIntensity, ANSI.Color) -> Box.Box -> Box.Box
colorCodeBox codeColor b = case codeColor of
  Nothing -> b
  Just cc
    | Box.rows b == 1 ->
        Box.text (ansiColor cc) Box.<> b `endWith` Box.text ansiColorReset

    | otherwise -> Box.hcat Box.left -- making two boxes, one for each side of the box so that it will set each row it's own color and will reset it afterwards
        [ Box.vcat Box.top $ replicate (Box.rows b) $ Box.text $ ansiColor cc
        , b
        , Box.vcat Box.top $ replicate (Box.rows b) $ Box.text ansiColorReset
        ]


-- | Default color intesity and color for code
defaultCodeColor :: (ANSI.ColorIntensity, ANSI.Color)
defaultCodeColor = (ANSI.Dull, ANSI.Yellow)

-- | `prettyPrintSingleError` Options
data PPEOptions = PPEOptions
  { ppeCodeColor         :: Maybe (ANSI.ColorIntensity, ANSI.Color) -- ^ Color code with this color... or not
  , ppeFull              :: Bool -- ^ Should write a full error message?
  , ppeLevel             :: Level -- ^ Should this report an error or a warning?
  , ppeShowDocs          :: Bool -- ^ Should show a link to error message's doc page?
  , ppeRelativeDirectory :: FilePath -- ^ FilePath to which the errors are relative
  }

-- | Default options for PPEOptions
defaultPPEOptions :: PPEOptions
defaultPPEOptions = PPEOptions
  { ppeCodeColor         = Just defaultCodeColor
  , ppeFull              = False
  , ppeLevel             = Error
  , ppeShowDocs          = True
  , ppeRelativeDirectory = mempty
  }

-- | Pretty print a single error, simplifying if necessary
prettyPrintSingleError :: PPEOptions -> ErrorMessage -> Box.Box
prettyPrintSingleError (PPEOptions codeColor full _level _showDocs relPath) e = flip evalState defaultUnknownMap $ do
  let em = if full then e else simplifyErrorMessage e
  um <- get
  return (prettyPrintErrorMessage um em)
  where
  (markCode, markCodeBox) = (colorCode &&& colorCodeBox) codeColor

  -- Pretty print an ErrorMessage
  prettyPrintErrorMessage :: TypeMap -> ErrorMessage -> Box.Box
  prettyPrintErrorMessage typeMap (ErrorMessage hints simple) =
    paras $
       foldr renderHint (indent (renderSimpleErrorMessage simple)) hints
       :
      maybe [] (return . Box.moveDown 1) typeInformation
    where
    typeInformation :: Maybe Box.Box
    typeInformation | not (null types) = Just $ Box.hsep 1 Box.left [ line "where", paras types ]
                    | otherwise = Nothing
      where
      types :: [Box.Box]
      types = map skolemInfo  (M.elems (umSkolemMap typeMap)) ++
              map unknownInfo (M.elems (umUnknownMap typeMap))

      skolemInfo :: (String, Int, Maybe SourceSpan) -> Box.Box
      skolemInfo (name, s, ss) =
        paras $
          line (markCode (T.pack (name <> show s)) <> " is a rigid type variable")
          : foldMap (return . line . ("  bound at " <>) . displayStartEndPos) ss

      unknownInfo :: Int -> Box.Box
      unknownInfo u = line $ markCode ("t" <> T.pack (show u)) <> " is an unknown type"

    renderSimpleErrorMessage :: SimpleErrorMessage -> Box.Box
    renderSimpleErrorMessage (InvalidFFIArity mn ident m n) =
      paras [ line $ "In the FFI module for " <> markCode (runModuleName mn) <> ":"
            , indent . paras $
                [ line $ "The identifier " <> markCode ident <> " was exported with (uncurried) arity " <> T.pack (show m)
                <> " but imported with (curried) arity " <> T.pack (show n) <> ". Export must have less than or equal arity."
                ]
            ]

    renderSimpleErrorMessage (FileIOError doWhat err) =
      paras [ line $ "I/O error while trying to " <> doWhat
            , indent . lineS $ displayException err
            ]

    renderSimpleErrorMessage (InternalError text) =
      paras [ line $ "Internal error: " <> text
            ]

    renderSimpleErrorMessage (MissingFFIModule mn) =
      line $ "The foreign module implementation for module " <> markCode (runModuleName mn) <> " is missing."

    renderSimpleErrorMessage (UnnecessaryFFIModule mn path) =
      paras [ line $ "An unnecessary foreign module implementation was provided for module " <> markCode (runModuleName mn) <> ": "
            , indent . lineS $ path
            , line $ "Module " <> markCode (runModuleName mn) <> " does not contain any foreign import declarations, so a foreign module is not necessary."
            ]

    renderSimpleErrorMessage (MissingFFIImplementations mn idents) =
      paras [ line $ "The following values are not defined in the foreign module for module " <> markCode (runModuleName mn) <> ": "
            , indent . paras $ map (line . runIdent) idents
            , line $ "Hint: Are you sure the file has valid Erlang syntax? Did you use -compile(export_all), which isn't supported yet? Did you write -exports instead of -export?"
            ]
    renderSimpleErrorMessage (UnusedFFIImplementations mn idents) =
      paras [ line $ "The following definitions in the foreign module for module " <> markCode (runModuleName mn) <> " are unused: "
            , indent . paras $ map (line . runIdent) idents
            ]


    renderHint :: ErrorMessageHint -> Box.Box -> Box.Box
    renderHint (ErrorUnifyingTypes t1@RCons{} t2@RCons{}) detail =
      let (row1Box, row2Box) = printRows t1 t2
      in paras [ detail
            , Box.hsep 1 Box.top [ line "while trying to match type"
                                 , row1Box
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "with type"
                                                   , row2Box
                                                   ]
            ]
    renderHint (ErrorUnifyingTypes t1 t2) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while trying to match type"
                                 , markCodeBox $ typeAsBox prettyDepth t1
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "with type"
                                                   , markCodeBox $ typeAsBox prettyDepth t2
                                                   ]
            ]
    renderHint (ErrorInExpression expr) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ Box.text "in the expression"
                                 , markCodeBox $ markCodeBox $ prettyPrintValue prettyDepth expr
                                 ]
            ]
    renderHint (ErrorInModule mn) detail =
      paras [ line $ "in module " <> markCode (runModuleName mn)
            , detail
            ]
    renderHint (ErrorInSubsumption t1 t2) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that type"
                                 , markCodeBox $ typeAsBox prettyDepth t1
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "is at least as general as type"
                                                   , markCodeBox $ typeAsBox prettyDepth t2
                                                   ]
            ]
    renderHint (ErrorInInstance nm ts) detail =
      paras [ detail
            , line "in type class instance"
            , markCodeBox $ indent $ Box.hsep 1 Box.top
               [ line $ showQualified runProperName nm
               , Box.vcat Box.left (map (typeAtomAsBox prettyDepth) ts)
               ]
            ]
    renderHint (ErrorCheckingKind ty kd) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that type"
                                 , markCodeBox $ typeAsBox prettyDepth ty
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "has kind"
                                                   , markCodeBox $ typeAsBox prettyDepth kd
                                                   ]
            ]
    renderHint (ErrorInferringKind ty) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while inferring the kind of"
                                 , markCodeBox $ typeAsBox prettyDepth ty
                                 ]
            ]
    renderHint ErrorCheckingGuard detail =
      paras [ detail
            , line "while checking the type of a guard clause"
            ]
    renderHint (ErrorInferringType expr) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while inferring the type of"
                                 , markCodeBox $ prettyPrintValue prettyDepth expr
                                 ]
            ]
    renderHint (ErrorCheckingType expr ty) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking that expression"
                                 , markCodeBox $ prettyPrintValue prettyDepth expr
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "has type"
                                                   , markCodeBox $ typeAsBox prettyDepth ty
                                                   ]
            ]
    renderHint (ErrorCheckingAccessor expr prop) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while checking type of property accessor"
                                 , markCodeBox $ prettyPrintValue prettyDepth (Accessor prop expr)
                                 ]
            ]
    renderHint (ErrorInApplication f t a) detail =
      paras [ detail
            , Box.hsep 1 Box.top [ line "while applying a function"
                                 , markCodeBox $ prettyPrintValue prettyDepth f
                                 ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "of type"
                                                   , markCodeBox $ typeAsBox prettyDepth t
                                                   ]
            , Box.moveRight 2 $ Box.hsep 1 Box.top [ line "to argument"
                                                   , markCodeBox $ prettyPrintValue prettyDepth a
                                                   ]
            ]
    renderHint (ErrorInDataConstructor nm) detail =
      paras [ detail
            , line $ "in data constructor " <> markCode (runProperName nm)
            ]
    renderHint (ErrorInTypeConstructor nm) detail =
      paras [ detail
            , line $ "in type constructor " <> markCode (runProperName nm)
            ]
    renderHint (ErrorInBindingGroup nms) detail =
      paras [ detail
            , line $ "in binding group " <> T.intercalate ", " (NEL.toList (fmap showIdent nms))
            ]
    renderHint (ErrorInDataBindingGroup nms) detail =
      paras [ detail
            , line $ "in data binding group " <> T.intercalate ", " (map runProperName nms)
            ]
    renderHint (ErrorInTypeSynonym name) detail =
      paras [ detail
            , line $ "in type synonym " <> markCode (runProperName name)
            ]
    renderHint (ErrorInValueDeclaration n) detail =
      paras [ detail
            , line $ "in value declaration " <> markCode (showIdent n)
            ]
    renderHint (ErrorInTypeDeclaration n) detail =
      paras [ detail
            , line $ "in type declaration for " <> markCode (showIdent n)
            ]
    renderHint (ErrorInTypeClassDeclaration name) detail =
      paras [ detail
            , line $ "in type class declaration for " <> markCode (runProperName name)
            ]
    renderHint (ErrorInKindDeclaration name) detail =
      paras [ detail
            , line $ "in kind declaration for " <> markCode (runProperName name)
            ]
    renderHint (ErrorInRoleDeclaration name) detail =
      paras [ detail
            , line $ "in role declaration for " <> markCode (runProperName name)
            ]
    renderHint (ErrorInForeignImport nm) detail =
      paras [ detail
            , line $ "in foreign import " <> markCode (showIdent nm)
            ]
    renderHint (ErrorInForeignImportData nm) detail =
      paras [ detail
            , line $ "in foreign data type declaration for " <> markCode (runProperName nm)
            ]
    renderHint (ErrorSolvingConstraint (Constraint _ nm _ ts _)) detail =
      paras [ detail
            , line "while solving type class constraint"
            , markCodeBox $ indent $ Box.hsep 1 Box.left
                [ line (showQualified runProperName nm)
                , Box.vcat Box.left (map (typeAtomAsBox prettyDepth) ts)
                ]
            ]
    renderHint (MissingConstructorImportForCoercible name) detail =
      paras
        [ detail
        , Box.moveUp 1 $ Box.moveRight 2 $ line $ "Solving this instance requires the newtype constructor " <> markCode (showQualified runProperName name) <> " to be in scope."
        ]
    renderHint (PositionedError srcSpan) detail =
      paras [ line $ "at " <> displaySourceSpan relPath (NEL.head srcSpan)
            , detail
            ]

    printRow :: (Int -> Type a -> Box.Box) -> Type a -> Box.Box
    printRow f t = markCodeBox $ indent $ f prettyDepth t

    -- If both rows are not empty, print them as diffs
    -- If verbose print all rows else only print unique rows
    printRows :: Type a -> Type a -> (Box.Box, Box.Box)
    printRows r1 r2 = case (full, r1, r2) of
      (True, _ , _) -> (printRow typeAsBox r1, printRow typeAsBox r2)

      (_, RCons{}, RCons{}) ->
        let (sorted1, sorted2) = filterRows (rowToList r1) (rowToList r2)
        in (printRow typeDiffAsBox sorted1, printRow typeDiffAsBox sorted2)

      (_, _, _) -> (printRow typeAsBox r1, printRow typeAsBox r2)


    -- Keep the unique labels only
    filterRows :: ([RowListItem a], Type a) -> ([RowListItem a], Type a) -> (Type a, Type a)
    filterRows (s1, r1) (s2, r2) =
         let sort' = sortOn (\(RowListItem _ name ty) -> (name, ty))
             notElem' s (RowListItem _ name ty) = all (\(RowListItem _ name' ty') -> name /= name' || not (eqType ty ty')) s
             unique1 = filter (notElem' s2) s1
             unique2 = filter (notElem' s1) s2
          in ( rowFromList (sort' unique1, r1)
             , rowFromList (sort' unique2, r2)
             )

  prettyDepth :: Int
  prettyDepth | full = 1000
              | otherwise = 3

  paras :: [Box.Box] -> Box.Box
  paras = Box.vcat Box.left

  -- | Simplify an error message
  simplifyErrorMessage :: ErrorMessage -> ErrorMessage
  simplifyErrorMessage (ErrorMessage hints simple) = ErrorMessage (simplifyHints hints) simple
    where
    -- Take the last instance of each "hint category"
    simplifyHints :: [ErrorMessageHint] -> [ErrorMessageHint]
    simplifyHints = reverse . nubBy categoriesEqual . stripRedudantHints simple . reverse

    -- Don't remove hints in the "other" category
    categoriesEqual :: ErrorMessageHint -> ErrorMessageHint -> Bool
    categoriesEqual x y =
      case (hintCategory x, hintCategory y) of
        (OtherHint, _) -> False
        (_, OtherHint) -> False
        (c1, c2) -> c1 == c2

    -- | See https://github.com/purescript/purescript/issues/1802
    stripRedudantHints :: SimpleErrorMessage -> [ErrorMessageHint] -> [ErrorMessageHint]
    stripRedudantHints _ = id

  hintCategory :: ErrorMessageHint -> HintCategory
  hintCategory ErrorCheckingType{}                  = ExprHint
  hintCategory ErrorInferringType{}                 = ExprHint
  hintCategory ErrorInExpression{}                  = ExprHint
  hintCategory ErrorUnifyingTypes{}                 = CheckHint
  hintCategory ErrorInSubsumption{}                 = CheckHint
  hintCategory ErrorInApplication{}                 = CheckHint
  hintCategory ErrorCheckingKind{}                  = CheckHint
  hintCategory ErrorSolvingConstraint{}             = SolverHint
  hintCategory PositionedError{}                    = PositionHint
  hintCategory _                                    = OtherHint

-- Pretty print and export declaration
prettyPrintExport :: DeclarationRef -> Text
prettyPrintExport (TypeRef _ pn _) = runProperName pn
prettyPrintExport ref =
  fromMaybe
    (internalError "prettyPrintRef returned Nothing in prettyPrintExport")
    (prettyPrintRef ref)

prettyPrintImport :: ModuleName -> ImportDeclarationType -> Maybe ModuleName -> Text
prettyPrintImport mn idt qual =
  let i = case idt of
            Implicit -> runModuleName mn
            Explicit refs -> runModuleName mn <> " (" <> T.intercalate ", " (mapMaybe prettyPrintRef refs) <> ")"
            Hiding refs -> runModuleName mn <> " hiding (" <> T.intercalate "," (mapMaybe prettyPrintRef refs) <> ")"
  in i <> maybe "" (\q -> " as " <> runModuleName q) qual


-- | Pretty print multiple errors
prettyPrintMultipleErrors :: PPEOptions -> MultipleErrors -> String
prettyPrintMultipleErrors ppeOptions = unlines . map renderBox . prettyPrintMultipleErrorsBox ppeOptions

-- | Pretty print multiple warnings
prettyPrintMultipleWarnings :: PPEOptions -> MultipleErrors -> String
prettyPrintMultipleWarnings ppeOptions = unlines . map renderBox . prettyPrintMultipleWarningsBox ppeOptions

-- | Pretty print warnings as a Box
prettyPrintMultipleWarningsBox :: PPEOptions -> MultipleErrors -> [Box.Box]
prettyPrintMultipleWarningsBox ppeOptions = prettyPrintMultipleErrorsWith (ppeOptions { ppeLevel = Warning }) "Warning found:" "Warning"

-- | Pretty print errors as a Box
prettyPrintMultipleErrorsBox :: PPEOptions -> MultipleErrors -> [Box.Box]
prettyPrintMultipleErrorsBox ppeOptions = prettyPrintMultipleErrorsWith (ppeOptions { ppeLevel = Error }) "Error found:" "Error"

prettyPrintMultipleErrorsWith :: PPEOptions -> String -> String -> MultipleErrors -> [Box.Box]
prettyPrintMultipleErrorsWith ppeOptions intro _ (MultipleErrors [e]) =
  let result = prettyPrintSingleError ppeOptions e
  in [ Box.vcat Box.left [ Box.text intro
                         , result
                         ]
     ]
prettyPrintMultipleErrorsWith ppeOptions _ intro (MultipleErrors es) =
  let result = map (prettyPrintSingleError ppeOptions) es
  in concat $ zipWith withIntro [1 :: Int ..] result
  where
  withIntro i err = [ Box.text (intro ++ " " ++ show i ++ " of " ++ show (length es) ++ ":")
                    , Box.moveRight 2 err
                    ]

-- | Pretty print a Parsec ParseError as a Box
prettyPrintParseError :: P.ParseError -> Box.Box
prettyPrintParseError = prettyPrintParseErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" . PE.errorMessages

-- | Pretty print 'ParseError' detail messages.
--
-- Adapted from 'Text.Parsec.Error.showErrorMessages'.
-- See <https://github.com/aslatter/parsec/blob/v3.1.9/Text/Parsec/Error.hs#L173>.
prettyPrintParseErrorMessages :: String -> String -> String -> String -> String -> [Message] -> Box.Box
prettyPrintParseErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
  | null msgs = Box.text msgUnknown
  | otherwise = Box.vcat Box.left $ map Box.text $ clean [showSysUnExpect,showUnExpect,showExpect,showMessages]

  where
  (sysUnExpect,msgs1) = span (SysUnExpect "" ==) msgs
  (unExpect,msgs2)    = span (UnExpect    "" ==) msgs1
  (expect,messages)   = span (Expect      "" ==) msgs2

  showExpect      = showMany msgExpecting expect
  showUnExpect    = showMany msgUnExpected unExpect
  showSysUnExpect | not (null unExpect) ||
                    null sysUnExpect = ""
                  | null firstMsg    = msgUnExpected ++ " " ++ msgEndOfInput
                  | otherwise        = msgUnExpected ++ " " ++ firstMsg
    where
    firstMsg  = PE.messageString (head sysUnExpect)

  showMessages      = showMany "" messages

  -- helpers
  showMany pre msgs' = case clean (map PE.messageString msgs') of
                         [] -> ""
                         ms | null pre  -> commasOr ms
                            | otherwise -> pre ++ " " ++ commasOr ms

  commasOr []       = ""
  commasOr [m]      = m
  commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

  commaSep          = separate ", " . clean

  separate   _ []     = ""
  separate   _ [m]    = m
  separate sep (m:ms) = m ++ sep ++ separate sep ms

  clean             = ordNub . filter (not . null)

-- | Indent to the right, and pad on top and bottom.
indent :: Box.Box -> Box.Box
indent = Box.moveUp 1 . Box.moveDown 1 . Box.moveRight 2

line :: Text -> Box.Box
line = Box.text . T.unpack

lineS :: String -> Box.Box
lineS = Box.text

renderBox :: Box.Box -> String
renderBox = unlines
            . map (dropWhileEnd isSpace)
            . dropWhile whiteSpace
            . dropWhileEnd whiteSpace
            . lines
            . Box.render
  where
  whiteSpace = all isSpace

toTypelevelString :: Type a -> Maybe Box.Box
toTypelevelString (TypeLevelString _ s) =
  Just . Box.text $ decodeStringWithReplacement s
toTypelevelString (TypeApp _ (TypeConstructor _ f) x)
  | f == primSubName C.typeError "Text" = toTypelevelString x
toTypelevelString (TypeApp _ (TypeConstructor _ f) x)
  | f == primSubName C.typeError "Quote" = Just (typeAsBox maxBound x)
toTypelevelString (TypeApp _ (TypeConstructor _ f) (TypeLevelString _ x))
  | f == primSubName C.typeError "QuoteLabel" = Just . line . prettyPrintLabel . Label $ x
toTypelevelString (TypeApp _ (TypeApp _ (TypeConstructor _ f) x) ret)
  | f == primSubName C.typeError "Beside" =
    (Box.<>) <$> toTypelevelString x <*> toTypelevelString ret
toTypelevelString (TypeApp _ (TypeApp _ (TypeConstructor _ f) x) ret)
  | f == primSubName C.typeError "Above" =
    (Box.//) <$> toTypelevelString x <*> toTypelevelString ret
toTypelevelString _ = Nothing

-- | Rethrow an error with a more detailed error message in the case of failure
rethrow :: (MonadError e m) => (e -> e) -> m a -> m a
rethrow f = flip catchError (throwError . f)

reifyErrors :: (MonadError e m) => m a -> m (Either e a)
reifyErrors ma = catchError (fmap Right ma) (return . Left)

reflectErrors :: (MonadError e m) => m (Either e a) -> m a
reflectErrors ma = ma >>= either throwError return

warnAndRethrow :: (MonadError e m, MonadWriter e m) => (e -> e) -> m a -> m a
warnAndRethrow f = rethrow f . censor f

-- | Rethrow an error with source position information
rethrowWithPosition :: (MonadError MultipleErrors m) => SourceSpan -> m a -> m a
rethrowWithPosition pos = rethrow (onErrorMessages (withPosition pos))

warnWithPosition :: (MonadWriter MultipleErrors m) => SourceSpan -> m a -> m a
warnWithPosition pos = censor (onErrorMessages (withPosition pos))

warnAndRethrowWithPosition :: (MonadError MultipleErrors m, MonadWriter MultipleErrors m) => SourceSpan -> m a -> m a
warnAndRethrowWithPosition pos = rethrowWithPosition pos . warnWithPosition pos

withPosition :: SourceSpan -> ErrorMessage -> ErrorMessage
withPosition NullSourceSpan err = err
withPosition pos (ErrorMessage hints se) = ErrorMessage (positionedError pos : hints) se

positionedError :: SourceSpan -> ErrorMessageHint
positionedError = PositionedError . pure

filterErrors :: (ErrorMessage -> Bool) -> MultipleErrors -> MultipleErrors
filterErrors f = MultipleErrors . filter f . runMultipleErrors

-- | Runs a computation listening for warnings and then escalating any warnings
-- that match the predicate to error status.
escalateWarningWhen
  :: (MonadWriter MultipleErrors m, MonadError MultipleErrors m)
  => (ErrorMessage -> Bool)
  -> m a
  -> m a
escalateWarningWhen isError ma = do
  (a, w) <- censor (const mempty) $ listen ma
  let (errors, warnings) = partition isError (runMultipleErrors w)
  tell $ MultipleErrors warnings
  unless (null errors) $ throwError $ MultipleErrors errors
  return a

-- | Collect errors in in parallel
parU
  :: forall m a b
   . MonadError MultipleErrors m
  => [a]
  -> (a -> m b)
  -> m [b]
parU xs f =
    forM xs (withError . f) >>= collectErrors
  where
    withError :: m b -> m (Either MultipleErrors b)
    withError u = catchError (Right <$> u) (return . Left)

    collectErrors :: [Either MultipleErrors b] -> m [b]
    collectErrors es = case partitionEithers es of
      ([], rs) -> return rs
      (errs, _) -> throwError $ fold errs
