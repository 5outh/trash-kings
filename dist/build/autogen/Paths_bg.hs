module Paths_bg (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Ben/.cabal/bin"
libdir     = "/Users/Ben/.cabal/lib/x86_64-osx-ghc-7.8.4/bg-0.1.0.0"
datadir    = "/Users/Ben/.cabal/share/x86_64-osx-ghc-7.8.4/bg-0.1.0.0"
libexecdir = "/Users/Ben/.cabal/libexec"
sysconfdir = "/Users/Ben/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bg_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bg_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bg_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bg_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bg_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
