module Document.Tests.IndirectEq where

import Document.Document

import UnitB.AST
import UnitB.PO

import Logic.Label
import Logic.Expr

import Z3.Z3 ( z3_code )

    -- Libraries
import Data.Map as M hiding (split, map)
import Data.String.Utils

import Control.Monad.Trans
import Control.Monad.Trans.Either

import Tests.UnitTest

import Utilities.Format
import Utilities.Syntactic

test_case :: TestCase
test_case = Case "train station example, with sets" test True

test :: IO Bool
test = test_cases
            [ Case "verify proof with galois connections" case0 result0
            , Case "verify theory 1: indirect (in)equality" case1 result1
            , Case "verify theory 2: lattices" case2 result2
            , Case "proofs by symmetry: PO" case3 result3
            , Case "proofs by symmetry: hypotheses labels" case4 result4
            , Case "cyclic references between proofs" case5 result5
            ]

path0 :: String
path0 = "tests/indirect-equality.tex"

path1 :: String
path1 = "tests/indirect-equality-t1.tex"

path2 :: String
path2 = "tests/indirect-equality-t2.tex"

case0 :: IO String
case0 = verify 0 path0

result0 :: String
result0 = unlines 
	[ " xxx m0/INIT/INV/inv0/assertion/indirect:eq/easy "
	, "  o  m0/INIT/INV/inv0/assertion/new:goal/goal "
	, "  o  m0/INIT/INV/inv0/assertion/new:goal/hypotheses "
	, "  o  m0/INIT/INV/inv0/assertion/new:goal/relation "
	, "  o  m0/INIT/INV/inv0/assertion/new:goal/step "
	, "  o  m0/INIT/INV/inv0/assertion/new:goal/step "
	, "  o  m0/INIT/INV/inv0/assertion/new:goal/step "
	, "  o  m0/INIT/INV/inv0/assertion/new:goal/step "
	, "  o  m0/INIT/INV/inv0/main goal/easy "
	, "  o  m0/INIT/INV/inv1/completeness "
	, "  o  m0/INIT/INV/inv1/part 1/easy "
	, "  o  m0/INIT/INV/inv1/part 2/goal "
	, "  o  m0/INIT/INV/inv1/part 2/hypotheses "
	, "  o  m0/INIT/INV/inv1/part 2/new assumption "
	, "  o  m0/INIT/INV/inv1/part 2/relation "
	, "  o  m0/INIT/INV/inv1/part 2/step "
	, "  o  m0/INIT/INV/inv1/part 2/step "
	, "  o  m0/INIT/INV/inv1/part 2/step "
	, "passed 17 / 18" ]
	
case1 :: IO String
case1 = verify_thy path0 "ctx0"

result1 :: String
result1 = unlines
    [ "  o  THM/thm0/completeness (131,1)"
    , "  o  THM/thm0/part 1/easy (133,2)"
    , "  o  THM/thm0/part 2/goal (143,2)"
    , "  o  THM/thm0/part 2/hypotheses (143,2)"
    , "  o  THM/thm0/part 2/new assumption (138,2)"
    , "  o  THM/thm0/part 2/relation (143,2)"
    , "  o  THM/thm0/part 2/step (145,2)"
    , "  o  THM/thm0/part 2/step (147,2)"
    , "  o  THM/thm0/part 2/step (150,2)"
    , "  o  THM/thm1/completeness (162,1)"
    , "  o  THM/thm1/part 1/goal (165,2)"
    , "  o  THM/thm1/part 1/hypotheses (165,2)"
    , "  o  THM/thm1/part 1/relation (165,2)"
    , "  o  THM/thm1/part 1/step (167,2)"
    , "  o  THM/thm1/part 2/goal (173,2)"
    , "  o  THM/thm1/part 2/hypotheses (173,2)"
    , "  o  THM/thm1/part 2/relation (173,2)"
    , "  o  THM/thm1/part 2/step (175,2)"
    , "  o  THM/thm1/part 2/step (177,2)"
    ]

case2 :: IO String
case2 = verify_thy path0 "ctx1"

result2 :: String
result2 = unlines
    [ "  o  THM/ctx1:thm3/goal (213,1)"
    , "  o  THM/ctx1:thm3/hypotheses (213,1)"
    , "  o  THM/ctx1:thm3/relation (213,1)"
    , "  o  THM/ctx1:thm3/step (216,1)"
    , "  o  THM/ctx1:thm3/step (219,1)"
    , "  o  THM/ctx1:thm3/step (221,1)"
    , "  o  THM/ctx1:thm4/assertion/indirect:eq/easy (236,24)"
    , "  o  THM/ctx1:thm4/assertion/new:goal/goal (237,41)"
    , "  o  THM/ctx1:thm4/assertion/new:goal/hypotheses (237,41)"
    , "  o  THM/ctx1:thm4/assertion/new:goal/relation (237,41)"
    , "  o  THM/ctx1:thm4/assertion/new:goal/step (240,1)"
    , "  o  THM/ctx1:thm4/assertion/new:goal/step (242,1)"
    , "  o  THM/ctx1:thm4/assertion/new:goal/step (244,1)"
    , " xxx THM/ctx1:thm4/assertion/new:goal/step (246,1)"
    , "  o  THM/ctx1:thm4/main goal/easy (236,24)"
    , "  o  THM/ctx1:thm5/goal (263,1)"
    , "  o  THM/ctx1:thm5/hypotheses (263,1)"
    , "  o  THM/ctx1:thm5/relation (263,1)"
    , "  o  THM/ctx1:thm5/step (266,1)"
    , "  o  THM/ctx1:thm5/step (268,1)"
    , "  o  THM/ctx1:thm5/step (270,1)"
    , "  o  THM/ctx1:thm5/step (272,1)"
    , "  o  THM/ctx1:thm6/goal (288,1)"
    , "  o  THM/ctx1:thm6/hypotheses (288,1)"
    , "  o  THM/ctx1:thm6/relation (288,1)"
    , "  o  THM/ctx1:thm6/step (291,1)"
    , "  o  THM/ctx1:thm6/step (293,1)"
    , "  o  THM/ctx1:thm6/step (295,1)"
    , "  o  THM/ctx1:thm6/step (297,1)"
    , "  o  THM/ctx1:thm7/goal (324,1)"
    , "  o  THM/ctx1:thm7/hypotheses (324,1)"
    , "  o  THM/ctx1:thm7/new assumption (313,1)"
    , "  o  THM/ctx1:thm7/relation (324,1)"
    , "  o  THM/ctx1:thm7/step (327,1)"
    , "  o  THM/ctx1:thm7/step (329,1)"
    , "  o  THM/ctx1:thm7/step (331,1)"
    , "  o  THM/ctx1:thm7/step (333,1)"
    , "  o  THM/ctx1:thm8/completeness (349,27)"
    , "  o  THM/ctx1:thm8/part 1/goal (357,1)"
    , "  o  THM/ctx1:thm8/part 1/hypotheses (357,1)"
    , "  o  THM/ctx1:thm8/part 1/relation (357,1)"
    , "  o  THM/ctx1:thm8/part 1/step (360,1)"
    , "  o  THM/ctx1:thm8/part 2/goal (372,27)"
    , "  o  THM/ctx1:thm8/part 2/hypotheses (372,27)"
    , "  o  THM/ctx1:thm8/part 2/new assumption (366,13)"
    , "  o  THM/ctx1:thm8/part 2/relation (372,27)"
    , "  o  THM/ctx1:thm8/part 2/step (375,1)"
    , "  o  THM/ctx1:thm8/part 2/step (377,1)"
    , " xxx THM/ctx1:thm9/assertion/symmetry/assertion/indirect:ineq/easy (398,33)"
    , "  o  THM/ctx1:thm9/assertion/symmetry/assertion/new:goal/goal (399,43)"
    , "  o  THM/ctx1:thm9/assertion/symmetry/assertion/new:goal/hypotheses (399,43)"
    , "  o  THM/ctx1:thm9/assertion/symmetry/assertion/new:goal/relation (399,43)"
    , "  o  THM/ctx1:thm9/assertion/symmetry/assertion/new:goal/step (402,1)"
    , " xxx THM/ctx1:thm9/assertion/symmetry/assertion/new:goal/step (404,1)"
    , "  o  THM/ctx1:thm9/assertion/symmetry/main goal/easy (398,33)"
    , "  o  THM/ctx1:thm9/assertion/symmetry/new assumption (397,38)"
    , "  o  THM/ctx1:thm9/main goal/case 1/easy (397,38)"
    , " xxx THM/ctx1:thm9/main goal/case 2/easy (397,38)"
    , "  o  THM/ctx1:thm9/main goal/completeness (397,38)"
    , "  o  THM/ctx1:thm9/new assumption (393,1)"
    ]

case3 :: IO String
case3 = verify_thy path1 "ctx2"

result3 :: String
result3 = unlines
    [ "  o  THM/thm4/case 1/easy (310,2)"
    , "  o  THM/thm4/case 2/assertion/symmetry/goal (315,2)"
    , "  o  THM/thm4/case 2/assertion/symmetry/hypotheses (315,2)"
    , "  o  THM/thm4/case 2/assertion/symmetry/new assumption (314,2)"
    , "  o  THM/thm4/case 2/assertion/symmetry/relation (315,2)"
    , "  o  THM/thm4/case 2/assertion/symmetry/step (317,2)"
    , "  o  THM/thm4/case 2/assertion/symmetry/step (319,2)"
    , " xxx THM/thm4/case 2/assertion/symmetry/step (321,2)"
    , "  o  THM/thm4/case 2/main goal/case 1/easy (314,2)"
    , "  o  THM/thm4/case 2/main goal/case 2/easy (314,2)"
    , "  o  THM/thm4/case 2/main goal/completeness (314,2)"
    , "  o  THM/thm4/completeness (308,1)"
    , "  o  THM/thm4/new assumption (300,23)"
    ]

case4 :: IO (Either [Error] String)
case4 = get_po "ctx2" $ label "THM/thm4/case 2/assertion/symmetry/easy (450,2)"

result4 :: Either a String
result4 = Right $ unlines
    [ "(declare-datatypes (a) ( (Maybe (Just (fromJust a)) Nothing) ))"
    , "(declare-datatypes () ( (Null null) ))"
    , "(declare-datatypes (a b) ( (Pair (pair (first a) (second b))) ))"
    , "; comment: we don't need to declare the sort Bool"
    , "; comment: we don't need to declare the sort Int"
    , "; comment: we don't need to declare the sort Real"
    , "(define-sort pfun (a b) (Array a (Maybe b)))"
    , "(define-sort set (a) (Array a Bool))"
    , "(declare-const i Int)"
    , "(declare-const j Int)"
    , "(declare-const k Int)"
    , "(declare-const swap (pfun (Pair (Pair Int Int) Int) Int))"
    , "(declare-fun apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "             ( (pfun (Pair (Pair Int Int) Int) Int)"
    , "               (Pair (Pair Int Int) Int) )"
    , "             Int)"
    , "(declare-fun dom-rest@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "             ( (set (Pair (Pair Int Int) Int))"
    , "               (pfun (Pair (Pair Int Int) Int) Int) )"
    , "             (pfun (Pair (Pair Int Int) Int) Int))"
    , "(declare-fun dom-subt@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "             ( (set (Pair (Pair Int Int) Int))"
    , "               (pfun (Pair (Pair Int Int) Int) Int) )"
    , "             (pfun (Pair (Pair Int Int) Int) Int))"
    , "(declare-fun dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "             ( (pfun (Pair (Pair Int Int) Int) Int) )"
    , "             (set (Pair (Pair Int Int) Int)))"
    , "(declare-fun elem@@Int (Int (set Int)) Bool)"
    , "(declare-fun elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close"
    , "             ( (Pair (Pair Int Int) Int)"
    , "               (set (Pair (Pair Int Int) Int)) )"
    , "             Bool)"
    , "(declare-fun empty-fun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "             ()"
    , "             (pfun (Pair (Pair Int Int) Int) Int))"
    , "(declare-fun empty-set@@Int () (set Int))"
    , "(declare-fun empty-set@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close"
    , "             ()"
    , "             (set (Pair (Pair Int Int) Int)))"
    , "(declare-fun intersect@@Int"
    , "             ( (set Int)"
    , "               (set Int) )"
    , "             (set Int))"
    , "(declare-fun intersect@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close"
    , "             ( (set (Pair (Pair Int Int) Int))"
    , "               (set (Pair (Pair Int Int) Int)) )"
    , "             (set (Pair (Pair Int Int) Int)))"
    , "(declare-fun mk-fun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "             ( (Pair (Pair Int Int) Int)"
    , "               Int )"
    , "             (pfun (Pair (Pair Int Int) Int) Int))"
    , "(declare-fun mk-set@@Int (Int) (set Int))"
    , "(declare-fun mk-set@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close"
    , "             ( (Pair (Pair Int Int) Int) )"
    , "             (set (Pair (Pair Int Int) Int)))"
    , "(declare-fun ovl@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "             ( (pfun (Pair (Pair Int Int) Int) Int)"
    , "               (pfun (Pair (Pair Int Int) Int) Int) )"
    , "             (pfun (Pair (Pair Int Int) Int) Int))"
    , "(declare-fun ran@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "             ( (pfun (Pair (Pair Int Int) Int) Int) )"
    , "             (set Int))"
    , "(declare-fun set-diff@@Int ( (set Int)   (set Int) ) (set Int))"
    , "(declare-fun set-diff@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close"
    , "             ( (set (Pair (Pair Int Int) Int))"
    , "               (set (Pair (Pair Int Int) Int)) )"
    , "             (set (Pair (Pair Int Int) Int)))"
    , "(declare-fun set@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "             ( (pfun (Pair (Pair Int Int) Int) Int) )"
    , "             (set Int))"
    , "(declare-fun subset@@Int ( (set Int)   (set Int) ) Bool)"
    , "(declare-fun subset@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close"
    , "             ( (set (Pair (Pair Int Int) Int))"
    , "               (set (Pair (Pair Int Int) Int)) )"
    , "             Bool)"
    , "(declare-fun tfun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "             ( (set (Pair (Pair Int Int) Int))"
    , "               (set Int) )"
    , "             (set (pfun (Pair (Pair Int Int) Int) Int)))"
    , "(declare-fun bunion@@Int ( (set Int)   (set Int) ) (set Int))"
    , "(declare-fun bunion@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close"
    , "             ( (set (Pair (Pair Int Int) Int))"
    , "               (set (Pair (Pair Int Int) Int)) )"
    , "             (set (Pair (Pair Int Int) Int)))"
    , "(assert (forall ( (f1 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (f2 (pfun (Pair (Pair Int Int) Int) Int)) )"
    , "                (=> true"
    , "                    (= (bunion@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1)"
    , "                                                                                (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f2))"
    , "                       (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (ovl@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 f2))))))"
    , "(assert (= (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int empty-fun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int)"
    , "           empty-set@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (mk-fun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int x y))"
    , "                       (mk-set@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x)))))"
    , "(assert (forall ( (f1 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (f2 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (x (Pair (Pair Int Int) Int)) )"
    , "                (=> true"
    , "                    (=> (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                               (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f2))"
    , "                        (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (ovl@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 f2)"
    , "                                                                                        x)"
    , "                           (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f2 x))))))"
    , "(assert (forall ( (f1 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (f2 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (x (Pair (Pair Int Int) Int)) )"
    , "                (=> true"
    , "                    (=> (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                               (set-diff@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1)"
    , "                                                                                                                                          (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f2)))"
    , "                        (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (ovl@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 f2)"
    , "                                                                                        x)"
    , "                           (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 x))))))"
    , "(assert (forall ( (f1 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (s1 (set (Pair (Pair Int Int) Int))) )"
    , "                (=> true"
    , "                    (= (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (dom-subt@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int s1 f1))"
    , "                       (set-diff@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1)"
    , "                                                                                  s1)))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (mk-fun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int x y)"
    , "                                                                                    x)"
    , "                       y))))"
    , "(assert (forall ( (f1 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (s1 (set (Pair (Pair Int Int) Int)))"
    , "                  (x (Pair (Pair Int Int) Int)) )"
    , "                (=> true"
    , "                    (=> (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                               (intersect@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close s1"
    , "                                                                                                                                           (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1)))"
    , "                        (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (dom-rest@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int s1 f1)"
    , "                                                                                        x)"
    , "                           (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 x))))))"
    , "(assert (forall ( (f1 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (s1 (set (Pair (Pair Int Int) Int)))"
    , "                  (x (Pair (Pair Int Int) Int)) )"
    , "                (=> true"
    , "                    (=> (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                               (set-diff@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1)"
    , "                                                                                                                                          s1))"
    , "                        (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (dom-subt@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int s1 f1)"
    , "                                                                                        x)"
    , "                           (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 x))))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int)) )"
    , "                (=> true"
    , "                    (= (select empty-fun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int"
    , "                               x)"
    , "                       (as Nothing (Maybe Int))))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (x2 (Pair (Pair Int Int) Int))"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (select (mk-fun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int x y)"
    , "                               x2)"
    , "                       (ite (= x x2) (Just y) (as Nothing (Maybe Int)))))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (f1 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (f2 (pfun (Pair (Pair Int Int) Int) Int)) )"
    , "                (=> true"
    , "                    (= (select (ovl@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 f2)"
    , "                               x)"
    , "                       (ite (= (select f2 x) (as Nothing (Maybe Int)))"
    , "                            (select f1 x)"
    , "                            (select f2 x))))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (f1 (pfun (Pair (Pair Int Int) Int) Int)) )"
    , "                (=> true"
    , "                    (= (select (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1)"
    , "                               x)"
    , "                       (not (= (select f1 x) (as Nothing (Maybe Int))))))))"
    , "(assert (forall ( (y Int)"
    , "                  (f1 (pfun (Pair (Pair Int Int) Int) Int)) )"
    , "                (=> true"
    , "                    (= (elem@@Int y"
    , "                                  (set@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1))"
    , "                       (exists ( (x (Pair (Pair Int Int) Int)) )"
    , "                               (and (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                                           (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1))"
    , "                                    (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 x)"
    , "                                       y)))))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (y Int)"
    , "                  (f1 (pfun (Pair (Pair Int Int) Int) Int)) )"
    , "                (=> true"
    , "                    (= (and (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                                   (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1))"
    , "                            (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 x)"
    , "                               y))"
    , "                       (= (select f1 x) (Just y))))))"
    , "(assert (forall ( (f1 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (x2 (Pair (Pair Int Int) Int))"
    , "                  (x (Pair (Pair Int Int) Int))"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (=> (not (= x x2))"
    , "                        (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (ovl@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1"
    , "                                                                                                                                                   (mk-fun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int x y))"
    , "                                                                                        x2)"
    , "                           (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 x2))))))"
    , "(assert (= (ran@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int empty-fun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int)"
    , "           empty-set@@Int))"
    , "(assert (forall ( (f1 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (elem@@Int y"
    , "                                  (ran@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1))"
    , "                       (exists ( (x (Pair (Pair Int Int) Int)) )"
    , "                               (and true"
    , "                                    (and (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                                                (dom@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1))"
    , "                                         (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 x)"
    , "                                            y))))))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (ran@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (mk-fun@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int x y))"
    , "                       (mk-set@@Int y)))))"
    , "(assert (forall ( (f1 (pfun (Pair (Pair Int Int) Int) Int))"
    , "                  (f2 (pfun (Pair (Pair Int Int) Int) Int)) )"
    , "                (=> true"
    , "                    (subset@@Int (ran@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int (ovl@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1 f2))"
    , "                                 (bunion@@Int (ran@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f1)"
    , "                                              (ran@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int f2))))))"
    , "(assert (forall ( (x Int)"
    , "                  (y Int) )"
    , "                (=> true (= (elem@@Int x (mk-set@@Int y)) (= x y)))))"
    , "(assert (forall ( (x Int)"
    , "                  (s1 (set Int))"
    , "                  (s2 (set Int)) )"
    , "                (=> true"
    , "                    (= (elem@@Int x (set-diff@@Int s1 s2))"
    , "                       (and (elem@@Int x s1) (not (elem@@Int x s2)))))))"
    , "(assert (forall ( (x Int)"
    , "                  (s1 (set Int))"
    , "                  (s2 (set Int)) )"
    , "                (=> true"
    , "                    (= (elem@@Int x (intersect@@Int s1 s2))"
    , "                       (and (elem@@Int x s1) (elem@@Int x s2))))))"
    , "(assert (forall ( (x Int)"
    , "                  (s1 (set Int))"
    , "                  (s2 (set Int)) )"
    , "                (=> true"
    , "                    (= (elem@@Int x (bunion@@Int s1 s2))"
    , "                       (or (elem@@Int x s1) (elem@@Int x s2))))))"
    , "(assert (forall ( (x Int) )"
    , "                (=> true (not (elem@@Int x empty-set@@Int)))))"
    , "(assert (forall ( (x Int)"
    , "                  (s1 (set Int)) )"
    , "                (=> true (= (elem@@Int x s1) (select s1 x)))))"
    , "(assert (forall ( (s1 (set Int))"
    , "                  (s2 (set Int)) )"
    , "                (=> true"
    , "                    (= (subset@@Int s1 s2)"
    , "                       (forall ( (x Int) )"
    , "                               (=> true (=> (elem@@Int x s1) (elem@@Int x s2))))))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (y (Pair (Pair Int Int) Int)) )"
    , "                (=> true"
    , "                    (= (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                              (mk-set@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close y))"
    , "                       (= x y)))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (s1 (set (Pair (Pair Int Int) Int)))"
    , "                  (s2 (set (Pair (Pair Int Int) Int))) )"
    , "                (=> true"
    , "                    (= (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                              (set-diff@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close s1 s2))"
    , "                       (and (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x s1)"
    , "                            (not (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x s2)))))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (s1 (set (Pair (Pair Int Int) Int)))"
    , "                  (s2 (set (Pair (Pair Int Int) Int))) )"
    , "                (=> true"
    , "                    (= (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                              (intersect@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close s1 s2))"
    , "                       (and (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x s1)"
    , "                            (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x s2))))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (s1 (set (Pair (Pair Int Int) Int)))"
    , "                  (s2 (set (Pair (Pair Int Int) Int))) )"
    , "                (=> true"
    , "                    (= (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                              (bunion@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close s1 s2))"
    , "                       (or (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x s1)"
    , "                           (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x s2))))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int)) )"
    , "                (=> true"
    , "                    (not (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x"
    , "                                                                                empty-set@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close)))))"
    , "(assert (forall ( (x (Pair (Pair Int Int) Int))"
    , "                  (s1 (set (Pair (Pair Int Int) Int))) )"
    , "                (=> true"
    , "                    (= (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x s1)"
    , "                       (select s1 x)))))"
    , "(assert (forall ( (s1 (set (Pair (Pair Int Int) Int)))"
    , "                  (s2 (set (Pair (Pair Int Int) Int))) )"
    , "                (=> true"
    , "                    (= (subset@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close s1 s2)"
    , "                       (forall ( (x (Pair (Pair Int Int) Int)) )"
    , "                               (=> true"
    , "                                   (=> (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x s1)"
    , "                                       (elem@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close x s2))))))))"
    , "; axm0"
    , "(assert (forall ( (i Int)"
    , "                  (j Int)"
    , "                  (k Int) )"
    , "                (=> true"
    , "                    (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int swap (pair (pair i j) k))"
    , "                       (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int swap (pair (pair j i) k))))))"
    , "; axm1"
    , "(assert (forall ( (i Int)"
    , "                  (j Int) )"
    , "                (=> true"
    , "                    (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int swap (pair (pair i j) i))"
    , "                       j))))"
    , "; axm2"
    , "(assert (forall ( (i Int)"
    , "                  (j Int) )"
    , "                (=> true"
    , "                    (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int swap (pair (pair i j) j))"
    , "                       i))))"
    , "; axm3"
    , "(assert (forall ( (i Int)"
    , "                  (j Int)"
    , "                  (k Int) )"
    , "                (=> (and (not (= k i)) (not (= k j)))"
    , "                    (= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int swap (pair (pair i j) k))"
    , "                       k))))"
    , "; hyp0"
    , "(assert (<= i k))"
    , "; hyp1"
    , "(assert (<= j k))"
    , "; hyp3"
    , "(assert (= i k))"
    , "(assert (not (<= (apply@Open@@Pair@Open@@Pair@@Int@@Int@Close@@Int@Close@@Int swap (pair (pair i j) k))"
    , "                 k)))"
    , "(check-sat-using (or-else (then qe smt)"
    , "                          (then simplify smt)"
    , "                          (then skip smt)"
    , "                          (then (using-params simplify :expand-power true) smt)))"
    ]

case5 :: IO String
case5 = parse path2

result5 :: String
result5 = "Left error (1,1): A cycle exists in the proofs of ctx1: thm3, thm5\n"

get_po :: FilePath -> Label -> IO (Either [Error] String)
get_po name lbl = runEitherT $ do
            s   <- EitherT $ parse_system path0
            pos <- hoistEither $ theory_po $ theories s ! name
            p   <- maybe 
                    (left $ [Error (format "unknown proof obligation: {0}" $ keys pos) 
                          $ LI path0 0 0]) 
                    right 
                    (M.lookup lbl pos)
            return $ concatMap pretty_print' $ z3_code p

parse :: FilePath -> IO String
parse path = do
        makeReport $ do
            EitherT $ parse_system path
            return "ok"
--        case r of
--            Right r -> do
--                return r
--            Left x -> return $ show x

verify_thy :: FilePath -> String -> IO String
verify_thy path name = do
        r <- runEitherT $ do
            s <- EitherT $ parse_system path
            pos <- hoistEither $ theory_po $ theories s ! name
            res <- lift $ verify_all pos
            return $ unlines $ map (\(k,r) -> success r ++ show k) $ toList res        
        case r of
            Right r -> do
                return r
            Left x -> return $ show x
    where
        success True  = "  o  "
        success False = " xxx "

verify :: Int -> FilePath -> IO String
verify n path = do
    r <- parse_machine path
    case r of
        Right ms -> do
            (s,_,_) <- str_verify_machine $ ms !! n
            return $ unlines $ map (head . split "(") $ lines s
        x -> return $ show x
