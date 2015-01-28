module Document.Tests.Phase where

    -- Modules
import Document.Tests.Suite

    -- Libraries
import Tests.UnitTest


test_case :: TestCase
test_case = Case "refinement relations in the phase example" test True

test :: IO Bool
test = test_cases 
            [ (Case "test 0, cyclic refinement relation between machines" (find_errors path0) result0)
            , (Case "test 1, valid references to variables and event declared in ancestor" case1 result1)
            ] 

path0 :: String
path0 = "tests/phases-t0.tex"

result0 :: String
result0 = "Error \"A cycle exists in the refinement structure: m0, m1\" (1,1)\n"

path1 :: String
path1 = "tests/phases-t1.tex"

case1 :: IO String
case1 = find_errors path1 

result1 :: String
result1 = "no errors"

