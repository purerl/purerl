{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Version where

import Data.Version (showVersion)
import Paths_purerl as Paths

#ifndef RELEASE
import qualified Development.GitRev as GitRev
#endif

versionString :: String
versionString = showVersion Paths.version ++ extra
  where
#ifdef RELEASE
  extra = " [purerl]"
#else
  extra = " [purerl; development build; commit: " ++ $(GitRev.gitHash) ++ dirty ++ "]"
  dirty
    | $(GitRev.gitDirty) = " DIRTY"
    | otherwise = ""
#endif
