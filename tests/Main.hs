module Main (main) where

import Prelude ()
import Prelude.Compat

import Test.Hspec

import qualified TestCompiler

import System.IO (hSetEncoding, stdout, stderr, utf8)
import qualified TestUtils

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  TestUtils.updateSupportCode

  hspec $ do
    beforeAll TestUtils.buildSupportModules $ do
      describe "compiler" TestCompiler.spec
