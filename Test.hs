{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad

import qualified UnitB.Test as UB
import qualified Latex.Test_Latex_Parser as LT
import qualified Z3.Test as ZT
import qualified Document.Test as DOC
--import qualified Utilities.Format as FMT
import qualified Utilities.Test as UT
import qualified Code.Test as Code

import Shelly

import System.Directory

import Tests.UnitTest

test_case :: IO Bool
test_case = run_test_cases $ test_cases 
        "Literate Unit-B Test Suite" 
        [  DOC.test_case
        ,  UB.test_case
        ,  LT.test_case
        ,  ZT.test_case
--        ,  FMT.test_case
        ,  UT.test_case
        ,  Code.test_case
        ]

main :: IO ()
main = do
    xs <- getDirectoryContents "."
    let prefix ys = ys `elem` map (take $ length ys) xs
    when (prefix "actual-") $
        shelly $ rm_f "actual-*.txt"
    when (prefix "expected-") $
        shelly $ rm_f "expected-*.txt"
    when (prefix "po-") $
        shelly $ rm_f "po-*.z3"
    b <- test_case
    if b 
    then do
        putStrLn "\n***************"
        putStrLn   "*** SUCCESS ***"
        putStrLn   "***************"
    else do
        putStrLn "\n***************"
        putStrLn   "*** FAILURE ***"
        putStrLn   "***************"
