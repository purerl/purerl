module Language.PureScript.Erl.Errors.Types where

import Prelude.Compat

import Language.PureScript.Names
import Data.Text
import Language.PureScript.AST.Declarations (ErrorMessageHint(..))

-- | A type of error messages
data SimpleErrorMessage
  = FileIOError Text IOError -- ^ A description of what we were trying to do, and the error which occurred
  | InvalidFFIArity ModuleName Text Int Int
  | MissingFFIModule ModuleName
  | UnnecessaryFFIModule ModuleName FilePath
  | MissingFFIImplementations ModuleName [Ident]
  | UnusedFFIImplementations ModuleName [Ident]
  deriving (Show)


data ErrorMessage = ErrorMessage
  [ErrorMessageHint]
  SimpleErrorMessage
  deriving (Show)