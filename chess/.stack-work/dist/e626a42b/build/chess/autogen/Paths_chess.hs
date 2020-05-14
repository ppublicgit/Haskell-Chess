{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_chess (
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

bindir     = "C:\\Users\\paul.abers\\Haskell\\Projects\\Chess\\chess\\.stack-work\\install\\c4f366b8\\bin"
libdir     = "C:\\Users\\paul.abers\\Haskell\\Projects\\Chess\\chess\\.stack-work\\install\\c4f366b8\\lib\\x86_64-windows-ghc-8.6.5\\chess-0.1.0.0-4V4Zhb3ftvd8l72yiscC5l-chess"
dynlibdir  = "C:\\Users\\paul.abers\\Haskell\\Projects\\Chess\\chess\\.stack-work\\install\\c4f366b8\\lib\\x86_64-windows-ghc-8.6.5"
datadir    = "C:\\Users\\paul.abers\\Haskell\\Projects\\Chess\\chess\\.stack-work\\install\\c4f366b8\\share\\x86_64-windows-ghc-8.6.5\\chess-0.1.0.0"
libexecdir = "C:\\Users\\paul.abers\\Haskell\\Projects\\Chess\\chess\\.stack-work\\install\\c4f366b8\\libexec\\x86_64-windows-ghc-8.6.5\\chess-0.1.0.0"
sysconfdir = "C:\\Users\\paul.abers\\Haskell\\Projects\\Chess\\chess\\.stack-work\\install\\c4f366b8\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chess_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chess_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "chess_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "chess_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chess_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chess_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
