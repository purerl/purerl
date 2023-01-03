{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Language.PureScript.Erl.Errors.JSON where

import Prelude.Compat

import qualified Data.Aeson.TH as A
import qualified Data.List.NonEmpty as NEL
import Data.Text (Text)

import qualified Language.PureScript as P
import Language.PureScript.Erl.Errors.Types
import Language.PureScript.Erl.Errors as E

data ErrorPosition = ErrorPosition
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  } deriving (Show, Eq, Ord)

data ErrorSuggestion = ErrorSuggestion
  { replacement :: Text
  , replaceRange :: Maybe ErrorPosition
  } deriving (Show, Eq)

data JSONError = JSONError
  { position :: Maybe ErrorPosition
  , message :: String
  , errorCode :: Text
  , errorLink :: Text
  , filename :: Maybe String
  , moduleName :: Maybe Text
  , allSpans :: [P.SourceSpan]
  } deriving (Show, Eq)

data JSONResult = JSONResult
  { warnings :: [JSONError]
  , errors :: [JSONError]
  } deriving (Show, Eq)

#ifndef __GHCIDE__
$(A.deriveJSON A.defaultOptions ''ErrorPosition)
$(A.deriveJSON A.defaultOptions ''JSONError)
$(A.deriveJSON A.defaultOptions ''JSONResult)
#endif

toJSONErrors :: Bool -> E.Level -> MultipleErrors -> [JSONError]
toJSONErrors verbose level = map (toJSONError verbose level) . E.runMultipleErrors

toJSONError :: Bool -> E.Level -> ErrorMessage -> JSONError
toJSONError verbose level e =
  JSONError (toErrorPosition <$> fmap NEL.head spans)
            (E.renderBox (E.prettyPrintSingleError (E.PPEOptions Nothing verbose level False mempty []) (E.stripModuleAndSpan e)))
            (E.errorCode e)
            (E.errorDocUri e)
            (P.spanName <$> fmap NEL.head spans)
            (P.runModuleName <$> E.errorModule e)
            (maybe [] NEL.toList spans)
  where
  spans :: Maybe (NEL.NonEmpty P.SourceSpan)
  spans = E.errorSpan e

  toErrorPosition :: P.SourceSpan -> ErrorPosition
  toErrorPosition ss =
    ErrorPosition (P.sourcePosLine   (P.spanStart ss))
                  (P.sourcePosColumn (P.spanStart ss))
                  (P.sourcePosLine   (P.spanEnd   ss))
                  (P.sourcePosColumn (P.spanEnd   ss))
