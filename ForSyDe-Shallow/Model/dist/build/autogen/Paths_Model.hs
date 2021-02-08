{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Model (
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

bindir     = "C:\\Users\\Marcus\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Marcus\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5\\Model-0.1.0.0-GFYMYwKZkGh6vDTo30xKMb"
dynlibdir  = "C:\\Users\\Marcus\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\Marcus\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.5\\Model-0.1.0.0"
libexecdir = "C:\\Users\\Marcus\\AppData\\Roaming\\cabal\\Model-0.1.0.0-GFYMYwKZkGh6vDTo30xKMb\\x86_64-windows-ghc-8.6.5\\Model-0.1.0.0"
sysconfdir = "C:\\Users\\Marcus\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Model_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Model_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Model_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Model_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Model_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Model_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
