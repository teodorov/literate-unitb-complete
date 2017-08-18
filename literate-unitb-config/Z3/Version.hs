{-# LANGUAGE LambdaCase #-}
module Z3.Version where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Precondition

import Data.Bidirectional
import Data.Char
import Data.HashMap.Strict (empty)
import Data.List as L
import Data.List.Utils as L
import Data.Yaml (Object, decodeFileEither)

import GHC.Generics

import System.Directory
import System.Environment
import System.FilePath
import System.Process
import System.IO.Unsafe

import Text.Pretty
import Text.Printf.TH

import Utilities.Config

data Z3Config = Z3Config 
    { _z3Path :: FilePath
    , _z3Timeout :: Int
    , _z3DefaultTimeout :: Int
    , _capacity :: Int }
    deriving (Show,Generic)

makeLenses ''Z3Config

instance NFData Z3Config where
instance PrettyRecord Z3Config where
instance PrettyPrintable Z3Config where
    pretty = prettyRecord

check_z3_bin :: IO Bool
check_z3_bin = do
    b <- z3_installed
    if b then do
        (v,h) <- z3_version
        let versions = [ ("4.3.2","2ca14b49fe45")
                       , ("4.3.2","784307fc3001")
                       , ("4.3.2","5e72cf0123f6")
                       , ("4.4.0","0482e7fe727c")
                       , ("4.4.1","e8811748d39a")
                       , ("4.4.1","")]
        if (v,h) `elem` versions then
            return True
        else do
            putStrLn $ [s|Expecting z3 %s\n|] $ intercalate " or\n"
                $ map (uncurry $ [s|z3 version %s, hashcode %s|]) versions
            return False
    else do
        putStrLn ("A 'z3' executable has not been found in the path ")
        return False

z3_version :: IO (String,String)
z3_version = do
        xs <- (words . head . lines) `liftM` readProcess z3_path ["--help"] ""
        let hashcode = dropWhile (/= "hashcode") xs^?ix 1
            version = dropWhile (/= "[version") xs ! 1
        return (version, maybe "" (filter isHexDigit) hashcode)


z3_installed :: IO Bool        
z3_installed = do
    path <- getEnv "PATH"
    xs   <- if is_os_windows then do
            let ps = L.split ";" path ++ ["."]
            forM ps (doesFileExist . (`combine` "z3.exe"))
    else do
            let ps = L.split ":" path
            forM ps (doesFileExist . (`combine` "z3"))
    z3_bin <- doesFileExist z3_path
    return $ or (z3_bin : xs)

config :: Lens' Object Z3Config
config = lensOf $ Z3Config 
        <$> fieldWith "z3" "z3_path" (view z3Path)
        <*> fieldWith' 20 "timeout" (view z3Timeout)
        <*> fieldWith' 3000 "default_timeout" (view z3DefaultTimeout)
        <*> fieldWith' 32 "capacity" (view capacity)

doesFileExist' :: FilePath -> IO (Maybe FilePath)
doesFileExist' fn = do
    doesFileExist fn >>= \case 
        True  -> return $ Just fn
        False -> return $ Nothing

doFilesExist :: [FilePath]
             -> IO (Maybe FilePath)
doFilesExist fs = do
    runMaybeT $ msum $ map (MaybeT . doesFileExist') fs

defaultConfigPath :: IO FilePath 
defaultConfigPath = do
    home <- homeSettingPath
    return $ home </> configFileName

configFileName :: FilePath
configFileName = "z3_config.yml"

getConfigFile :: IO (FilePath, Object)
getConfigFile = do
    path <- getExecutablePath
    def  <- defaultConfigPath
    let fn = configFileName
    doFilesExist 
            [ fn 
            , path </> fn 
            , def ]
        >>= \case
            Just fn' -> (fn',) . fromRight empty <$> decodeFileEither fn'
            Nothing  -> return $ (def, empty)

z3_config :: Z3Config
z3_config = unsafePerformIO $ do
    cp <- snd <$> getConfigFile
    return $ cp^.config

z3_path :: String
z3_path = z3_config^.z3Path

default_timeout :: Int
default_timeout = z3_config^.z3DefaultTimeout
