{-# LANGUAGE DeriveFunctor,RankNTypes,FlexibleInstances #-}
module Main where

import Control.DeepSeq
import Control.Exception
import Control.Lens as L

import Data.ConfigFile hiding (set)
import Data.Maybe
import Data.Monoid

import Options.Applicative 

import System.Directory

import Utilities.Config
import Z3.Version

data SetOptions = SetOptions
        { newZ3Path :: Maybe FilePath 
        , newHardTimeout :: Maybe Int 
        , newDefaultTimeout :: Maybe Int 
        , newCapacity :: Maybe Int }
    deriving Show

commandLineOpts :: Parser SetOptions
commandLineOpts = SetOptions
    <$> optional (strOption $ 
            long "z3-path" 
         <> metavar "Z3_PATH" 
         <> help "the path to call z3" )
    <*> optional (option auto $ 
            long "hard-timeout" 
         <> metavar "HARD_TIMEOUT" 
         <> help "hard maximum of z3 run time (s)" )
    <*> optional (option auto $ 
            long "default-timeout" 
         <> metavar "DEFAULT_TIMEOUT" 
         <> help "soft maximum of z3 run time (ms)" )
    <*> optional (option auto $ 
            long "capacity" 
         <> metavar "CAPACITY" 
         <> help "number of provers being run simultaneous" )

rewriteConfig :: (Z3Config -> Z3Config)
              -> IO ()
rewriteConfig f = do
    _ <- evaluate $ force z3_config
    putStrLn "options were:"
    print z3_config
    (fn,cp) <- getConfigFile
    _ <- evaluate cp
    let c' = f $ cp^.config
    homeSetting <- homeSettingPath
    createDirectoryIfMissing False homeSetting
    writeFile fn $ to_string $ cp & config .~ c'
    putStrLn "set the options to:"
    print c'

viewConfig :: IO ()
viewConfig = print z3_config

apply :: SetOptions -> IO ()
apply opt = do
    fromMaybe viewConfig
        $ rewriteConfig . appEndo <$> 
                   (Endo . set z3Path <$> newZ3Path opt)
                <> (Endo . set z3Timeout <$> newHardTimeout opt)
                <> (Endo . set z3DefaultTimeout <$> newDefaultTimeout opt)
                <> (Endo . set capacity <$> newCapacity opt)

main :: IO ()
main = execParser opts >>= apply
    where
        opts = info ( helper <*> commandLineOpts )
                $   fullDesc
                 <> header  "unitb-setting - customize the preferences for verifier"
