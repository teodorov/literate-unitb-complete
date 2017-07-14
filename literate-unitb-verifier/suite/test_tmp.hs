{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.UnitB.Parser as Doc ( syntaxSummary )
import Language.UnitB.Parser.Phase.Expressions as PExp
import Language.UnitB.Parser.MachineSpec as MSpec
import Language.UnitB.Parser.Tests.Cubes   as Cubes
import Language.UnitB.Parser.Tests.Definitions  as Def
import Language.UnitB.Parser.Tests.GarbageCollector  as GC
import Language.UnitB.Parser.Tests.Lambdas as Lam
import Language.UnitB.Parser.Tests.LockFreeDeque as Deq
import Language.UnitB.Parser.Tests.Parser  as Parser
import Language.UnitB.Parser.Tests.Phase   as Sync
import Language.UnitB.Parser.Tests.Puzzle  as Puzz
import Language.UnitB.Parser.Tests.SmallMachine  as SM
import Language.UnitB.Parser.Tests.Suite hiding (proof_obligation)
import Language.UnitB.Parser.Tests.TerminationDetection  as Term
import Language.UnitB.Parser.Tests.TrainStation     as TS
import Language.UnitB.Parser.Tests.TrainStationRefinement  as TSRef
import Language.UnitB.Parser.Tests.TrainStationSets as TSS
import Logic.Expr
import Language.UnitB.Parser.Phase.Test as Ph
import Language.UnitB.Parser.Test as Doc
import Utilities.Test as Ut
import Language.UnitB.Test as UB
-- import Language.UnitB as UB hiding (raw_proof_obligation)
-- import Logic.Expr.PrettyPrint
-- import Logic.Names
import Logic.Names.Packaged ()
-- import Logic.Proof
-- import Language.UnitB.Test as UB
--import Latex.Parser
-- import qualified Language.UnitB.Parser.Test as DOC
import qualified Utilities.Test as UT
import qualified Code.Test as Code
import qualified Documentation.Test as Sum

import Test.UnitTest

-- import Language.Haskell.TH
-- import Language.Haskell.TH.Syntax

import Control.Concurrent
import Control.Monad

-- import System.FilePath.Lens

import System.Process
-- import System.Timeout

-- import qualified Utilities.Lines as Lines
import Utilities.TimeIt
-- import Utilities.Timeout
-- import Utilities.Map

import Test.QuickCheck hiding (label)
import Test.QuickCheck.Report

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Language.UnitB.Parser.ExprScope as EScope

main :: IO ()
main = timeIt $ void $ do
    setNumCapabilities 8
    _ <- system "rm actual-*.txt"
    _ <- system "rm expected-*.txt"
    _ <- system "rm po-*.z3"
    _ <- system "rm log*.z3"
    T.writeFile "syntax.txt" $ T.unlines syntaxSummary
    putStrLn $ nameType
    _ <- return $ raw_proof_obligation Deq.path1 "m0/INIT/FIS/q/p" 0
    _ <- return $ printQuickCheckResult MSpec.run_spec
    _ <- return $ quickCheck MSpec.prop_expr_parser
    _ <- return $ run_test_cases Deq.test_case
    -- timeIt $ do
    --     p <- parse_system path
    --     evaluate $ force p
    -- x <- proof_obligation Deq.path4 "m1/LIVE/m1:prog3/ensure/TR/m0:pop:left:empty/NEG" 1
    _ <- return $ run_test_cases Term.test_case
    _ <- return $ run_test_cases Ph.test_case
    _ <- return $ run_test_cases Ut.test_case
    ----print =<< Ph.case7
    _ <- return $ run_test_cases Code.test_case
    _ <- return $ run_test_cases Sum.test_case
    _ <- return $ print =<< run_test_cases Doc.check_axioms
    _ <- return $ printQuickCheckResult PExp.check_props
    _ <- return $ run_test_cases SM.test_case
-- ******
    _ <- return $ run_test_cases Lam.test_case
-- ******
    _ <- return $ run_test_cases Cubes.test_case
    _ <- return $ run_test_cases Sync.test_case
    _ <- return $ run_test_cases Puzz.test_case
    _ <- return $ quickCheck MSpec.prop_expr_parser
    _ <- return $ printQuickCheckResult MSpec.run_spec
    _ <- return $ print =<< run_test_cases check_axioms
    _ <- return $ run_test_cases Def.test_case
    -- timeout (60 * 1000000) $ do
    _ <- return $ run_test_cases UB.test_case
    -- _ <- return $ print =<< Lines.run_tests
    _ <- return $ run_test_cases TS.test_case
    _ <- return $ run_test_cases TSS.test_case
    _ <- return $ run_test_cases TSRef.test_case
    _ <- return $ run_test_cases UT.test_case
    _ <- return $ run_test_cases GC.test_case
    _ <- return $ run_test_cases Parser.test_case
    _ <- return $ run_test_cases Doc.test_case
    _ <- return $ printQuickCheckResult EScope.run_tests
    return ()
