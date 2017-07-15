{-# LANGUAGE CPP #-}
module Language.UnitB.Parser.Tests.TrainStationRefinement 
    ( test, test_case, path3 )
where

    -- Modules
import Language.UnitB.Parser.Tests.Suite

    -- Libraries
#if MIN_VERSION_semigroups(0,18,0)
import Data.List.NonEmpty as NE
#else
import Data.List.NonEmpty as NE hiding (unlines)
#endif
import           Data.Text (Text)
import qualified Data.Text as T

import Test.UnitTest

test_case :: TestCase
test_case = test

test :: TestCase
test = test_cases
            "train station example, with refinement"
            [ poCase "verify machine m0 (ref)" (verify path0 0) result0
            , poCase "verify machine m1 (ref)" (verify path0 1) result1
            , textCase "Feasibility in m1" case6 result6
            , poCase "verify machine m2 (ref)" (verify path0 2) result2
            , poCase "verify machine m2 (ref), in many files" 
                (verifyFiles (NE.fromList [path1,path1']) 2) result2
            , textCase "cyclic proof of liveness through 3 refinements" (find_errors path3) result3
            , textCase "refinement of undefined machine" (find_errors path4) result4
            , textCase "repeated imports" case5 result5
            ]

result0 :: Output
result0 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationRefinement/result0.txt|]

result1 :: Output
result1 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationRefinement/result1.txt|]

result2 :: Output
result2 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationRefinement/result2.txt|]

path0 :: FilePath
path0 = [path|Tests/train-station-ref.tex|]

path1 :: FilePath
path1 = [path|Tests/train-station-ref/main.tex|]

path1' :: FilePath
path1' = [path|Tests/train-station-ref/ref0.tex|]

path3 :: FilePath
path3 = [path|Tests/train-station-ref-err0.tex|]

result3 :: Output
result3 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationRefinement/result3.txt|]

path4 :: FilePath
path4 = [path|Tests/train-station-ref-err1.tex|]

result4 :: Output
result4 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationRefinement/result4.txt|]

-- parse :: FilePath -> IO Text
-- parse path = do
--     r <- parse_machine path
--     return $ case r of
--         Right _ -> "ok"
--         Left xs -> T.unlines $ map report xs

path5 :: FilePath
path5 = [path|Tests/train-station-ref-err2.tex|]

result5 :: Output
result5 = readFileLn' "expected/Language/UnitB/Parser/TrainStationRefinement/result5.txt"
    T.unlines
    [ "Theory imported multiple times"
    , "error 38:1:"
    , "\tsets"
    , ""
    , "error 88:1:"
    , "\tsets"
    , ""
    , "error 444:1:"
    , "\tsets"
    , ""
    , "error 445:1:"
    , "\tsets"
    , ""
    , ""
    , "Theory imported multiple times"
    , "error 89:1:"
    , "\tfunctions"
    , ""
    , "error 446:1:"
    , "\tfunctions"
    , ""
    ]

case5 :: IO Text
case5 = find_errors path5

case6 :: IO Text
case6 = proof_obligation path0 "m1/m1:moveout/FIS/loc@prime" 1

result6 :: Output
result6 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationRefinement/result6.txt|]
