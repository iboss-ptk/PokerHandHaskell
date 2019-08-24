{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_PokerHandHaskell (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/supanat/Library/Haskell/bin"
libdir     = "/Users/supanat/Library/Haskell/ghc-8.0.2-x86_64/lib/PokerHandHaskell-0.1.0.0"
dynlibdir  = "/Users/supanat/Library/Haskell/ghc-8.0.2-x86_64/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/supanat/Library/Haskell/share/ghc-8.0.2-x86_64/PokerHandHaskell-0.1.0.0"
libexecdir = "/Users/supanat/Library/Haskell/libexec"
sysconfdir = "/Users/supanat/Library/Haskell/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PokerHandHaskell_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PokerHandHaskell_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "PokerHandHaskell_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "PokerHandHaskell_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PokerHandHaskell_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PokerHandHaskell_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
