#!/usr/bin/env cabal exec runhaskell
module Main where

import Data.Function
import Data.List

import           Pipes
import           Pipes.Safe

import System.Directory

import Text.Printf

import Tools.Clipboard
import Tools.Quote

import Utilities.Config hiding (quote)

main :: IO ()
main = do
    ls <- getDirectoryContents "."
    let xs = filter (isPrefixOf "actual-") ls
        ys = filter (isPrefixOf "expected-") ls
    fix (\proc n xs ys -> do
        print xs
        print ys
        if null xs || null ys then return ()
        else do
            let file1 = (printf "actual-%d.txt" n)
                file2 = (printf "expected-%d.txt" (n :: Int))
            b1 <- doesFileExist file1
            b2 <- doesFileExist file2
            if b1 && b2 then do
                h <- head.lines <$> readFile file2
                putStrLn h
                -- system $ "script/quote.hs \"" ++ file1 ++ "\" | pbcopy"
                runEffect $ runSafeP $ quote file1 >-> copyToClipboard
                diff file1 file2
                return ()
            else return ()
            proc (n+1) (delete file1 xs) (delete file2 ys)
        ) 0 xs ys
