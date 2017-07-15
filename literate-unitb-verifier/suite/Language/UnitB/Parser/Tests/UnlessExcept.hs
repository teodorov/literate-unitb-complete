module Language.UnitB.Parser.Tests.UnlessExcept where

    -- Modules
import Language.UnitB.Parser.Tests.Suite

import Test.UnitTest

test_case :: TestCase
test_case = test

test :: TestCase
test = test_cases
            "Unless / except clause"
            [ (poCase "test 0, unless/except without indices" 
                (verify path0 0) result0)
            , (poCase "test 1, unless/except with indices and free variables" 
                (verify path0 1) result1)
            ]

path0 :: FilePath
path0 = [path|Tests/unless-except.tex|]

result0 :: Output
result0 = readFileLn [path|expected/Language/UnitB/Parser/UnlessExcept/result0.txt|]

result1 :: Output
result1 = readFileLn [path|expected/Language/UnitB/Parser/UnlessExcept/result1.txt|]
