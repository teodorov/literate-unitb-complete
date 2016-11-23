module Paths_classy_lens_hierarchy (
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

bindir     = "/Users/simon/Documents/Haskell/Literate Unit-B/GitHub/branches/generalize-po-gen/literate-unitb/.cabal-sandbox/bin"
libdir     = "/Users/simon/Documents/Haskell/Literate Unit-B/GitHub/branches/generalize-po-gen/literate-unitb/.cabal-sandbox/lib/x86_64-osx-ghc-7.10.3/classy-lens-hierarchy-0.1.0.0-EKPNw319fwU4fhU5GIzjb0"
datadir    = "/Users/simon/Documents/Haskell/Literate Unit-B/GitHub/branches/generalize-po-gen/literate-unitb/.cabal-sandbox/share/x86_64-osx-ghc-7.10.3/classy-lens-hierarchy-0.1.0.0"
libexecdir = "/Users/simon/Documents/Haskell/Literate Unit-B/GitHub/branches/generalize-po-gen/literate-unitb/.cabal-sandbox/libexec"
sysconfdir = "/Users/simon/Documents/Haskell/Literate Unit-B/GitHub/branches/generalize-po-gen/literate-unitb/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "classy_lens_hierarchy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "classy_lens_hierarchy_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "classy_lens_hierarchy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "classy_lens_hierarchy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "classy_lens_hierarchy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
