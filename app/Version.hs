{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Version where

import Prelude

import Data.Version (showVersion)
import Paths_purerl as Paths

#if !(defined RELEASE) && !(defined __GHCIDE__)
import qualified Development.GitRev as GitRev
#endif

versionString :: String
versionString = showVersion Paths.version ++ extra
  where
#if defined(RELEASE) || defined(__GHCIDE__)
  extra = ""
#else
  extra = " [development build; commit: " ++ $(GitRev.gitHash) ++ dirty ++ "]"
  dirty
    | $(GitRev.gitDirty) = " DIRTY"
    | otherwise = ""
#endif
