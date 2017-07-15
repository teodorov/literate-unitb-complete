module Language.UnitB.Parser.Tests.TrainStationSets where

import Language.UnitB.Parser.Tests.Suite

    -- Libraries
import           Data.Text (Text)

import Test.UnitTest

test_case :: TestCase
test_case = test

test :: TestCase
test = test_cases
            "train station example, with sets"
            [ poCase "verify machine m0 (sets)" (verify path0 0) result0
            , poCase "verify machine m1 (sets)" (verify path0 1) result1
            , poCase "verify machine m2 (sets)" (verify path0 2) result2
            , textCase "m2, enablement proof obligation" case6 result6
            , textCase "m2, transient proof obligation, feasibility" case7 result7
            , textCase "m2, transient proof obligation, enablement" case10 result10
            , textCase "m2, transient proof obligation, negation" case11 result11
            , textCase "m2, safety property of schedule replacement" case8 result8
            , poCase "verify machine m3 (sets)" (verify path0 3) result5
            , textCase "type checking of boolean expressions" case3 result3
            , textCase "verify machine m3 error (wandering free variable)" case4 result4
            , textCase "witness well definedness PO" case9 result9
            , textCase "PO in m3 (sets)" case12 result12
            ]

result0 :: Output
result0 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result0.txt|]

result1 :: Output
result1 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result1.txt|]

result2 :: Output
result2 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result2.txt|]

path0 :: FilePath
path0 = [path|Tests/train-station-set.tex|]

result3 :: Output
result3 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result3.txt|]

path3 :: FilePath
path3 = [path|Tests/train-station-set-err0.tex|]

case3 :: IO Text
case3 = find_errors path3

path4 :: FilePath
path4 = [path|Tests/train-station-set-err1.tex|]

case4 :: IO Text
case4 = find_errors path4

result4 :: Output
result4 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result4.txt|]

result5 :: Output
result5 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result5.txt|]

case6 :: IO Text
case6 = proof_obligation path0 "m2/m2:tr1/TR/m1:moveout/EN" 2

result6 :: Output
result6 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result6.txt|]

case7 :: IO Text
case7 = proof_obligation path0 "m2/m2:tr0/TR/WFIS/t/t@prime" 2

result7 :: Output
result7 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result7.txt|]

case10 :: IO Text
case10 = proof_obligation path0 "m2/m2:tr0/TR/m0:leave/EN" 2

result10 :: Output
result10 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result10.txt|]

case11 :: IO Text
case11 = proof_obligation path0 "m2/m2:tr0/TR/m0:leave/NEG" 2

result11 :: Output
result11 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result11.txt|]

case8 :: IO Text
case8 = proof_obligation path0 "m3/m1:moveout/C_SCH/delay/0/saf/m1:moveout/SAF/m1:moveout" 3

result8 :: Output
result8 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result8.txt|]

case9 :: IO Text
case9 = proof_obligation path0 "m3/m3:tr0/TR/WD/witness/p" 3

result9 :: Output
result9 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result9.txt|]

case12 :: IO Text
case12 = proof_obligation path0 "m3/m3:ctr:plf/FIS/osgn@prime" 3

result12 :: Output
result12 = readFileLn [path|expected/Language/UnitB/Parser/TrainStationSets/result12.txt|]
