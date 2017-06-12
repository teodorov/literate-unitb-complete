#!/usr/bin/env cabal exec runhaskell
{-# LANGUAGE QuasiQuotes #-}
import Control.Monad
import Control.Lens.Extras

import Data.List
import Data.List.Lens

import System.Directory
import System.Process

import Text.Printf.TH

main :: IO ()
main = do
        ls <- getDirectoryContents "."
        let xs = filter (isPrefixOf "po-") ls ++ filter (is $ prefixed "log".suffixed ".z3") ls
        forM_ xs $ \fn -> do
            putStrLn fn
            _ <- system ([s|head -n 2 %s|] fn)
            system ([s|time z3 -smt2 -T:30 %s|] fn)

