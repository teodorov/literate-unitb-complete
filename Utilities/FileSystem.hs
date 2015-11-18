{-# LANGUAGE ImplicitParams #-}
module Utilities.FileSystem 
    ( FileSystem (..)
    , MockFileSystem
    , NoParam(..)
    , OneParam(..)
    , MockFileSystemState
    , MockFileSystemState' (..)
    , ifFileExists'
    , files
    , module System.Directory 
    , runMockFileSystem
    , runMockFileSystem'
    , execMockFileSystem'
    , execMockFileSystem
    , evalMockFileSystem'
    , evalMockFileSystem
    )
where

import Control.Lens
import Control.Monad.State

import Data.Default
import Data.Map as M
import Data.Maybe

import Prelude hiding (readFile,writeFile)
import qualified Prelude as P

import System.Directory hiding 
            ( createDirectoryIfMissing
            , doesDirectoryExist
            , doesFileExist)
import qualified System.Directory as D
import System.FileLock
import System.FilePath

import Utilities.CallStack
import Utilities.Invariant
import Utilities.Partial

newtype ExistingFile s = ExistingFile FilePath

newtype NoParam r m = NoParam { getNoParam :: m r }
newtype OneParam a r m = OneParam { getOneParam :: (forall s. ExistingFile s -> m a) -> m r }

--class Runnable (f :: (* -> *) -> *) m0 where
--    type Internal m0 :: *
--    run :: f (Internal m0) -> f m0

class Monad m => FileSystem m where
    {-# MINIMAL liftFS, lift2FS | 
        readFile, writeFile
        , createDirectoryIfMissing
        , ifFileExists, doesFileExist
        , doesDirectoryExist #-}
    readFile  :: ExistingFile s -> m String
    readFile fn = getNoParam $ liftFS (NoParam $ readFile fn)
    writeFile :: (?loc :: CallStack) => FilePath -> String -> m ()
    writeFile fn xs = getNoParam $ liftFS (NoParam $ writeFile fn xs)
    ifFileExists :: FilePath -> (forall s. ExistingFile s -> m a) -> m (Maybe a) 
    ifFileExists fn = getOneParam $ lift2FS (OneParam $ ifFileExists fn)
    createDirectoryIfMissing :: Bool -> FilePath -> m ()
    createDirectoryIfMissing b fn = getNoParam $ liftFS (NoParam $ createDirectoryIfMissing b fn)
    doesDirectoryExist :: FilePath -> m Bool
    doesDirectoryExist fn = getNoParam $ liftFS (NoParam $ doesDirectoryExist fn)
    doesFileExist :: FilePath -> m Bool
    doesFileExist fn = getNoParam $ liftFS (NoParam $ doesFileExist fn)
    liftFS :: (?loc :: CallStack) 
           => (forall m0. (?loc :: CallStack, FileSystem m0) => NoParam a m0) 
           -> NoParam a m
    liftFS f = f
    lift2FS :: (?loc :: CallStack) 
            => (forall m0. (?loc :: CallStack, FileSystem m0) => OneParam a r m0) 
            -> OneParam a r m
    lift2FS f = f

ifFileExists' :: FileSystem m
              => FilePath -> a 
              -> (forall s. ExistingFile s -> m a) 
              -> m a
ifFileExists' fn x cmd = fromMaybe x <$> ifFileExists fn cmd

instance FileSystem IO where
    readFile (ExistingFile fn) = do
        P.readFile fn
    writeFile = P.writeFile
    ifFileExists fn cmd = do
        b <- doesFileExist fn
        if b then do
            Just <$> withFileLock fn Shared (const $ cmd $ ExistingFile fn)
        else do
            return Nothing
    createDirectoryIfMissing = D.createDirectoryIfMissing
    doesDirectoryExist = D.doesDirectoryExist
    doesFileExist = D.doesFileExist

newtype MockFileSystem a = MockFileSystem 
        (State MockFileSystemState a)
    deriving (Functor,Applicative,Monad)

newtype MockFileSystemState' = MockFileSystemState 
        { _files :: Map FilePath (Maybe String) }
    deriving (Show)

type MockFileSystemState = Checked MockFileSystemState'

makeLenses ''MockFileSystemState'

instance HasInvariant MockFileSystemState' where
    invariant m = do        
        "inv0" ## traverseWithKey 
            (\f _ -> f ## takeDirectory f `M.lookup` (m^.files) === Just Nothing) 
            (m^.files)
        "has current directory" ## "." `M.lookup` (m^.files) === Just Nothing
        "has root" ## "/" `M.lookup` (m^.files) === Just Nothing

instance FileSystem MockFileSystem where
    readFile (ExistingFile fn) = MockFileSystem $ fromJust' <$> uses' files (! fn)
    writeFile fn x = do
        dirExists  <- doesDirectoryExist (takeDirectory fn)
        isDir      <- doesDirectoryExist fn
        MockFileSystem $ mutate' assert $ do 
            --entry <- uses files (takeDirectory fn `M.lookup`)
            --file  <- uses files (fn `M.lookup`)
            providedM (dirExists && not isDir)  $
                files %= insert fn (Just x)
    ifFileExists fn f = do
            exists <- doesFileExist fn
            if exists
                then Just <$> f (ExistingFile fn)
                else return Nothing
    doesFileExist fn = MockFileSystem $ 
            (Just True ==).fmap isJust <$> uses' files (fn `M.lookup`)
    doesDirectoryExist fn = MockFileSystem $ 
            (Just True ==).fmap isNothing <$> uses' files (fn `M.lookup`)
    createDirectoryIfMissing b fn = do
            exists <- doesDirectoryExist (takeDirectory fn)
            providedM (b || exists) $ do
                MockFileSystem $ mutate' assert $ do
                    let ds 
                            | b         = takeWhile (`notElem` [".","/"]) $ iterate takeDirectory fn
                            | otherwise = [fn]
                    files %= union (fromList $ (,Nothing) <$> ds)

emptyFSState :: MockFileSystemState 
emptyFSState = create' assert $ do
        files .= fromList [(".",Nothing),("/",Nothing)]

instance Default MockFileSystemState' where
    def = emptyFSState^.content'

runMockFileSystem :: MockFileSystem a -> (a,MockFileSystemState)
runMockFileSystem cmd = runMockFileSystem' cmd emptyFSState
evalMockFileSystem :: MockFileSystem a -> a
evalMockFileSystem = fst . runMockFileSystem
execMockFileSystem :: MockFileSystem a -> MockFileSystemState
execMockFileSystem = snd . runMockFileSystem

runMockFileSystem' :: MockFileSystem a -> MockFileSystemState -> (a,MockFileSystemState)
runMockFileSystem' (MockFileSystem cmd) = runState cmd
evalMockFileSystem' :: MockFileSystem a -> MockFileSystemState -> a
evalMockFileSystem' (MockFileSystem cmd) = evalState cmd
execMockFileSystem' :: MockFileSystem a -> MockFileSystemState -> MockFileSystemState
execMockFileSystem' (MockFileSystem cmd) = execState cmd