module Document.Tests.Definitions where

    -- Modules
import Document.Tests.Suite as S

import Test.UnitTest

    -- Libraries
import           Data.Text (Text)
import qualified Data.Text as T

test_case :: TestCase
test_case = test

test :: TestCase
test = test_cases
            "Specification and refinement of a lock-free algorithm"
            [ textCase "parsing and using definitions" case0 result0 
            , poCase "proving using definitions" case1 result1 
            , textCase "invariance proof obligation" case2 result2 ]

path0 :: FilePath
path0 = [path|Tests/definitions/definitions.tex|]

case0 :: IO Text
case0 = do
    find_errors path0

result0 :: Text
result0 = "no errors"

case1 :: IO POResult
case1 = verify path0 0

result1 :: Text
result1 = T.unlines
    [ "  o  m0/INIT/INV/inv0"
    , " xxx m0/act/INV/inv0"
    , "passed 1 / 2"
    ]

case2 :: IO Text
case2 = proof_obligation path0 "m0/act/INV/inv0" 0

result2 :: Text
result2 = T.unlines
    [ "; m0/act/INV/inv0"
    , "(set-option :auto-config false)"
    , "(set-option :smt.timeout 3000)"
    , "(declare-datatypes (a) ( (Maybe (Just (fromJust a)) Nothing) ))"
    , "(declare-datatypes () ( (Null null) ))"
    , "(declare-datatypes (a b) ( (Pair (pair (first a) (second b))) ))"
    , "(define-sort guarded (a) (Maybe a))"
    , "; comment: we don't need to declare the sort Bool"
    , "; comment: we don't need to declare the sort Int"
    , "; comment: we don't need to declare the sort Real"
    , "(define-sort set (a) (Array a Bool))"
    , "(declare-const x Int)"
    , "(declare-const x@prime Int)"
    , "(declare-const y Int)"
    , "(declare-const y@prime Int)"
    , "(define-fun foo () Bool (<= x y))"
    , "(define-fun foo@prime () Bool (<= x@prime y@prime))"
    , "; SKIP:y"
    , "(assert (= y@prime y))"
    , "; act0"
    , "(assert (= x@prime (+ x 1)))"
    , "; inv0"
    , "(assert foo)"
    , "(assert (not foo@prime))"
    , "(check-sat-using (or-else (then qe smt)"
    , "                          (then simplify smt)"
    , "                          (then skip smt)"
    , "                          (then (using-params simplify :expand-power true) smt)))"
    , "; m0/act/INV/inv0"
    ]
