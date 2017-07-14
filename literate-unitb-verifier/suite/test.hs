{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Invariant
import Control.Lens 
import Control.Monad

import Data.Functor.Compose
-- import Data.List
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Language.UnitB.Parser hiding (system)
import Language.UnitB.Parser.Tests.SmallMachine as SM
import qualified Language.UnitB.Test as UB
import Logic.UnitTest 
import qualified Language.UnitB.Parser.Test as DOC
import qualified Utilities.Test as UT

-- import Shelly hiding (time,get)

import Options.Applicative

-- import System.Directory
import System.Exit
import System.Process

import Test.UnitTest hiding (QuickCheckProps)
import Test.QuickCheck.Lens 

import Utilities.TimeIt
import Test.QuickCheck.ZoomEq
import Text.Pretty
import Z3.Version

selected_test_case :: TestCase
selected_test_case = test_cases 
        "Selected Literate Unit-B Test Case" 
        [ UB.test_case ]

test_case :: TestCase
test_case = test_cases 
        "Literate Unit-B Test Suite" 
        [  DOC.test_case
        ,  UB.test_case
--        ,  FMT.test_case
        ,  UT.test_case
        ]

data TestSelection = QuickCheckOnly Int | All Int | POCasesOnly

selectTestCase :: Parser TestCase
selectTestCase = flag Main.test_case selected_test_case
        (  long "selected-case" 
        <> help "Execute only the hard coded test case selection" )

executionMode :: Parser TestSelection
executionMode = 
        flag' POCasesOnly
            (  long "po-only" 
            <> help "among all the test cases, only run the verification test cases and check the PO" )
    <|> (   flag' QuickCheckOnly 
              (  long "quickcheck-only" 
              <> help "run only the QuickCheck properites." )
        <|> pure All)
        <*> (option auto 
                (  short 'c'
                <> metavar "TEST-COUNT"
                <> help "TEST-COUNT specifies the number of examples to test. Default to 100 " ) 
            <|> pure 100)

runSelection :: TestSelection -> TestCase -> IO Bool
runSelection (All n) t = run_test_cases_with t $ argMaxSuccess .= n
runSelection (QuickCheckOnly n) t = run_quickCheck_suite_with t $ argMaxSuccess .= n
runSelection POCasesOnly t = run_poTestSuite t

testScript :: IO Bool
testScript = do
    x <- SM.case0
    let x' = x & mapped.mapped %~ getCompose
        m' = getCompose SM.m0_machine
        m  = SM.m0_machine
    print $ x  == Right [SM.m0_machine]
    print $ x' == Right [m']
    print $ invariantMessage $ x  .== Right [m]
    print $ invariantMessage $ x' .== Right [m']
    return True


parseSelection :: Parser (IO Bool)
parseSelection = 
            flag' testScript
                (  long "select-script" 
                <> help "run hard coded test script" ) 
        <|> runSelection <$> executionMode <*> selectTestCase

trashTestFiles :: IO ()
trashTestFiles = do
    -- xs <- getDirectoryContents "."
    void $ system "rm actual* expected* po-* log*.z3"

main :: IO ()
main = timeIt $ do
    let opts = info (helper <*> parseSelection)
          ( fullDesc
         <> progDesc "Test Literate Unit-B"
         <> header "test - the Literate Unit-B test suite" )
    T.writeFile "syntax.txt" $ T.unlines syntaxSummary
    trashTestFiles
    setNumCapabilities 8
    putStrLn $ pretty z3_config
    -- b <- run_quickCheck_suite_with Main.test_case $ argMaxSuccess .= 1000
    -- b <- run_poTestSuite Main.test_case
    b <- join $ execParser opts
    if b 
    then do
        putStrLn "\n***************"
        putStrLn   "*** SUCCESS ***"
        putStrLn   "***************"
        exitSuccess
    else do
        putStrLn "\n***************"
        putStrLn   "*** FAILURE ***"
        putStrLn   "***************"
        exitFailure
