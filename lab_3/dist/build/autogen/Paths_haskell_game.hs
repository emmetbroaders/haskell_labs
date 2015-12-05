module Paths_haskell_game (
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
version = Version [0,3,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/spoot/.cabal/bin"
libdir     = "/home/spoot/.cabal/lib/x86_64-linux-ghc-7.10.2/haskell-game-0.3.0.0-4rUjiAYnDy2HYBvAa6LX0K"
datadir    = "/home/spoot/.cabal/share/x86_64-linux-ghc-7.10.2/haskell-game-0.3.0.0"
libexecdir = "/home/spoot/.cabal/libexec"
sysconfdir = "/home/spoot/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_game_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_game_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "haskell_game_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_game_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_game_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
