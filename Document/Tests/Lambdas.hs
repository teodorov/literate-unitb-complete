module Document.Tests.Lambdas where

    -- Modules
import Document.Document

import Logic.Expr
import Logic.Proof

import Theories.FunctionTheory

import UnitB.AST
import UnitB.PO

import Z3.Z3 hiding ( verify )

    -- Libraries
import Control.Monad.Trans.Either

import Data.Map hiding ( map )

import Tests.UnitTest

import Utilities.Syntactic

test_case :: TestCase
test_case = Case "lambda expressions in the cube example" test True

test :: IO Bool
test = test_cases
            [ (Case "part 0" part0 True)
            , (Case "part 1" part1 True) 
            , (Case "part 2" part2 True) 
            , (Case "part 3" part3 True) 
            ]            
part0 :: IO Bool
part0 = test_cases
            [ (POCase "test 0, verification, lambda vs empty-fun" 
                (verify path0) result0)
            , (POCase "test 1, verification, lambda vs ovl, mk-fun" 
                (verify path1) result1)
            , (POCase "test 2, verification, lambda vs apply" 
                (verify path2) result2)
            ]            
part1 :: IO Bool
part1 = test_cases
            [ (POCase "test 3, verification, set comprehension, failed proof" 
                (verify path3) result3)
            , (Case "test 4, adding a progress property" case4 result4)
            , (Case "test 5, unless properties" case5 result5)
            ]            
part2 :: IO Bool
part2 = test_cases
            [ (POCase "test 6, verify progress refinement" case6 result6)
            , (POCase "test 7, verify refinement rules" case7 result7)
            , (POCase "test 8, verify refinement rules" case8 result8)
            ]            
part3 :: IO Bool
part3 = test_cases
            [ (POCase "test 9, verify disjunction rule" (verify path9) result9)
            , (POCase "test 10, error: cyclic proof" (verify path10) result10)
            , (StringCase   "test 11, intermediate goals of monotonic \
                              \simplification" case11 result11)
            , (Case "test 12, bound variable with ambiguous type"
                case12 result12)
            ]

result0 :: String
result0 = unlines 
    [ "  o  m0/INIT/FIS/a"
    , "  o  m0/INIT/FIS/b"
    , "  o  m0/INIT/FIS/c"
    , "  o  m0/INIT/FIS/f"
    , "  o  m0/INIT/FIS/n"
    , "  o  m0/INIT/INV/inv0"
    , "  o  m0/INIT/INV/inv1"
    , "  o  m0/INIT/INV/inv2"
    , "  o  m0/INIT/INV/inv3/goal (221,7)"
    , "  o  m0/INIT/INV/inv3/hypotheses (221,7)"
    , "  o  m0/INIT/INV/inv3/relation (221,7)"
    , "  o  m0/INIT/INV/inv3/step (223,1)"
    , "  o  m0/INIT/INV/inv3/step (225,1)"
    , "  o  m0/INIT/INV/inv3/step (229,1)"
    , "  o  m0/INIT/WD"
    , "  o  m0/INV/WD"
    , "  o  m0/evt/FIS/a@prime"
    , "  o  m0/evt/FIS/b@prime"
    , "  o  m0/evt/FIS/c@prime"
    , "  o  m0/evt/FIS/f@prime"
    , "  o  m0/evt/FIS/n@prime"
    , "  o  m0/evt/INV/inv0/goal (63,7)"
    , "  o  m0/evt/INV/inv0/hypotheses (63,7)"
    , "  o  m0/evt/INV/inv0/relation (63,7)"
    , "  o  m0/evt/INV/inv0/step (65,1)"
    , "  o  m0/evt/INV/inv0/step (67,1)"
    , "  o  m0/evt/INV/inv0/step (69,1)"
    , "  o  m0/evt/INV/inv0/step (71,1)"
    , "  o  m0/evt/INV/inv0/step (73,1)"
    , "  o  m0/evt/INV/inv1/goal (141,7)"
    , "  o  m0/evt/INV/inv1/hypotheses (141,7)"
    , "  o  m0/evt/INV/inv1/relation (141,7)"
    , "  o  m0/evt/INV/inv1/step (143,1)"
    , "  o  m0/evt/INV/inv1/step (145,1)"
    , "  o  m0/evt/INV/inv1/step (147,1)"
    , "  o  m0/evt/INV/inv1/step (149,1)"
    , "  o  m0/evt/INV/inv1/step (151,1)"
    , "  o  m0/evt/INV/inv1/step (153,1)"
    , "  o  m0/evt/INV/inv1/step (155,1)"
    , "  o  m0/evt/INV/inv2/easy (190,1)"
    , " xxx m0/evt/INV/inv3"
    , "  o  m0/evt/SCH"
    , "  o  m0/evt/WD/ACT/a0"
    , "  o  m0/evt/WD/ACT/a1"
    , "  o  m0/evt/WD/ACT/a2"
    , "  o  m0/evt/WD/ACT/a3"
    , "  o  m0/evt/WD/C_SCH"
    , "  o  m0/evt/WD/F_SCH"
    , "  o  m0/evt/WD/GRD"
    , "passed 48 / 49"
    ]

path0 :: String
path0 = "tests/cubes-t0.tex"

result1 :: String
result1 = unlines
    [ "  o  m0/INIT/FIS/a"
    , "  o  m0/INIT/FIS/b"
    , "  o  m0/INIT/FIS/c"
    , "  o  m0/INIT/FIS/f"
    , "  o  m0/INIT/FIS/n"
    , "  o  m0/INIT/INV/inv0"
    , "  o  m0/INIT/INV/inv1"
    , "  o  m0/INIT/INV/inv2"
    , "  o  m0/INIT/INV/inv3/goal (221,7)"
    , "  o  m0/INIT/INV/inv3/hypotheses (221,7)"
    , "  o  m0/INIT/INV/inv3/relation (221,7)"
    , "  o  m0/INIT/INV/inv3/step (223,1)"
    , "  o  m0/INIT/INV/inv3/step (225,1)"
    , "  o  m0/INIT/INV/inv3/step (229,1)"
    , "  o  m0/INIT/INV/inv4"
    , "  o  m0/INIT/WD"
    , "  o  m0/INV/WD"
    , "  o  m0/evt/FIS/a@prime"
    , "  o  m0/evt/FIS/b@prime"
    , "  o  m0/evt/FIS/c@prime"
    , "  o  m0/evt/FIS/f@prime"
    , "  o  m0/evt/FIS/n@prime"
    , "  o  m0/evt/INV/inv0/goal (63,7)"
    , "  o  m0/evt/INV/inv0/hypotheses (63,7)"
    , "  o  m0/evt/INV/inv0/relation (63,7)"
    , "  o  m0/evt/INV/inv0/step (65,1)"
    , "  o  m0/evt/INV/inv0/step (67,1)"
    , "  o  m0/evt/INV/inv0/step (69,1)"
    , "  o  m0/evt/INV/inv0/step (71,1)"
    , "  o  m0/evt/INV/inv0/step (73,1)"
    , "  o  m0/evt/INV/inv1/goal (141,7)"
    , "  o  m0/evt/INV/inv1/hypotheses (141,7)"
    , "  o  m0/evt/INV/inv1/relation (141,7)"
    , "  o  m0/evt/INV/inv1/step (143,1)"
    , "  o  m0/evt/INV/inv1/step (145,1)"
    , "  o  m0/evt/INV/inv1/step (147,1)"
    , "  o  m0/evt/INV/inv1/step (149,1)"
    , "  o  m0/evt/INV/inv1/step (151,1)"
    , "  o  m0/evt/INV/inv1/step (153,1)"
    , "  o  m0/evt/INV/inv1/step (155,1)"
    , "  o  m0/evt/INV/inv2/easy (190,1)"
    , "  o  m0/evt/INV/inv3/goal (240,7)"
    , "  o  m0/evt/INV/inv3/hypotheses (240,7)"
    , "  o  m0/evt/INV/inv3/relation (240,7)"
    , "  o  m0/evt/INV/inv3/step (242,1)"
    , "  o  m0/evt/INV/inv3/step (244,1)"
    , "  o  m0/evt/INV/inv3/step (246,1)"
    , "  o  m0/evt/INV/inv3/step (248,1)"
    , "  o  m0/evt/INV/inv3/step (250,1)"
    , "  o  m0/evt/INV/inv3/step (252,1)"
    , "  o  m0/evt/INV/inv4"
    , "  o  m0/evt/SCH"
    , "  o  m0/evt/WD/ACT/a0"
    , "  o  m0/evt/WD/ACT/a1"
    , "  o  m0/evt/WD/ACT/a2"
    , "  o  m0/evt/WD/ACT/a3"
    , "  o  m0/evt/WD/ACT/a4"
    , "  o  m0/evt/WD/C_SCH"
    , "  o  m0/evt/WD/F_SCH"
    , "  o  m0/evt/WD/GRD"
    , "passed 60 / 60"
    ]

path1 :: String
path1 = "tests/cubes-t1.tex"

result2 :: String
result2 = unlines
    [ "  o  m0/INIT/FIS/a"
    , "  o  m0/INIT/FIS/b"
    , "  o  m0/INIT/FIS/c"
    , "  o  m0/INIT/FIS/f"
    , "  o  m0/INIT/FIS/n"
    , "  o  m0/INIT/INV/inv0"
    , "  o  m0/INIT/INV/inv1"
    , "  o  m0/INIT/INV/inv2"
    , "  o  m0/INIT/INV/inv3/goal (222,7)"
    , "  o  m0/INIT/INV/inv3/hypotheses (222,7)"
    , "  o  m0/INIT/INV/inv3/relation (222,7)"
    , "  o  m0/INIT/INV/inv3/step (224,1)"
    , "  o  m0/INIT/INV/inv3/step (226,1)"
    , "  o  m0/INIT/INV/inv3/step (230,1)"
    , "  o  m0/INIT/INV/inv4"
    , "  o  m0/INIT/INV/inv5"
    , "  o  m0/INIT/WD"
    , "  o  m0/INV/WD"
    , "  o  m0/evt/FIS/a@prime"
    , "  o  m0/evt/FIS/b@prime"
    , "  o  m0/evt/FIS/c@prime"
    , "  o  m0/evt/FIS/f@prime"
    , "  o  m0/evt/FIS/n@prime"
    , "  o  m0/evt/INV/inv0/goal (64,7)"
    , "  o  m0/evt/INV/inv0/hypotheses (64,7)"
    , "  o  m0/evt/INV/inv0/relation (64,7)"
    , "  o  m0/evt/INV/inv0/step (66,1)"
    , "  o  m0/evt/INV/inv0/step (68,1)"
    , "  o  m0/evt/INV/inv0/step (70,1)"
    , "  o  m0/evt/INV/inv0/step (72,1)"
    , "  o  m0/evt/INV/inv0/step (74,1)"
    , "  o  m0/evt/INV/inv1/goal (142,7)"
    , "  o  m0/evt/INV/inv1/hypotheses (142,7)"
    , "  o  m0/evt/INV/inv1/relation (142,7)"
    , "  o  m0/evt/INV/inv1/step (144,1)"
    , "  o  m0/evt/INV/inv1/step (146,1)"
    , "  o  m0/evt/INV/inv1/step (148,1)"
    , "  o  m0/evt/INV/inv1/step (150,1)"
    , "  o  m0/evt/INV/inv1/step (152,1)"
    , "  o  m0/evt/INV/inv1/step (154,1)"
    , "  o  m0/evt/INV/inv1/step (156,1)"
    , "  o  m0/evt/INV/inv2/easy (191,1)"
    , "  o  m0/evt/INV/inv3/goal (241,7)"
    , "  o  m0/evt/INV/inv3/hypotheses (241,7)"
    , "  o  m0/evt/INV/inv3/relation (241,7)"
    , "  o  m0/evt/INV/inv3/step (243,1)"
    , "  o  m0/evt/INV/inv3/step (245,1)"
    , "  o  m0/evt/INV/inv3/step (247,1)"
    , "  o  m0/evt/INV/inv3/step (253,1)"
    , "  o  m0/evt/INV/inv3/step (255,1)"
    , "  o  m0/evt/INV/inv4"
    , "  o  m0/evt/INV/inv5/assertion/asm0/easy (298,1)"
    , "  o  m0/evt/INV/inv5/main goal/goal (279,7)"
    , "  o  m0/evt/INV/inv5/main goal/hypotheses (279,7)"
    , "  o  m0/evt/INV/inv5/main goal/relation (279,7)"
    , "  o  m0/evt/INV/inv5/main goal/step (281,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (283,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (285,1)"
--     , " xxx m0/evt/INV/inv5/main goal/step (287,1)"
           -- as expected
    , "  o  m0/evt/INV/inv5/main goal/step (287,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (290,1)"
    , "  o  m0/evt/SCH"
    , "  o  m0/evt/WD/ACT/a0"
    , "  o  m0/evt/WD/ACT/a1"
    , "  o  m0/evt/WD/ACT/a2"
    , "  o  m0/evt/WD/ACT/a3"
    , "  o  m0/evt/WD/ACT/a4"
    , "  o  m0/evt/WD/C_SCH"
    , "  o  m0/evt/WD/F_SCH"
    , "  o  m0/evt/WD/GRD"
    , "passed 69 / 69"
    ]

path2 :: String
path2 = "tests/cubes-t2.tex"

result3 :: String
result3 = unlines
    [ "  o  m0/INIT/FIS/a"
    , "  o  m0/INIT/FIS/b"
    , "  o  m0/INIT/FIS/c"
    , "  o  m0/INIT/FIS/f"
    , "  o  m0/INIT/FIS/n"
    , "  o  m0/INIT/INV/inv0"
    , "  o  m0/INIT/INV/inv1"
    , "  o  m0/INIT/INV/inv2"
    , "  o  m0/INIT/INV/inv3/goal (222,7)"
    , "  o  m0/INIT/INV/inv3/hypotheses (222,7)"
    , "  o  m0/INIT/INV/inv3/relation (222,7)"
    , "  o  m0/INIT/INV/inv3/step (224,1)"
    , "  o  m0/INIT/INV/inv3/step (226,1)"
    , "  o  m0/INIT/INV/inv3/step (230,1)"
    , "  o  m0/INIT/INV/inv4"
    , "  o  m0/INIT/INV/inv5"
    , "  o  m0/INIT/INV/inv6"
    , "  o  m0/INIT/WD"
    , "  o  m0/INV/WD"
    , "  o  m0/evt/FIS/a@prime"
    , "  o  m0/evt/FIS/b@prime"
    , "  o  m0/evt/FIS/c@prime"
    , "  o  m0/evt/FIS/f@prime"
    , "  o  m0/evt/FIS/n@prime"
    , "  o  m0/evt/INV/inv0/goal (64,7)"
    , "  o  m0/evt/INV/inv0/hypotheses (64,7)"
    , "  o  m0/evt/INV/inv0/relation (64,7)"
    , "  o  m0/evt/INV/inv0/step (66,1)"
    , "  o  m0/evt/INV/inv0/step (68,1)"
    , "  o  m0/evt/INV/inv0/step (70,1)"
    , "  o  m0/evt/INV/inv0/step (72,1)"
    , "  o  m0/evt/INV/inv0/step (74,1)"
    , "  o  m0/evt/INV/inv1/goal (142,7)"
    , "  o  m0/evt/INV/inv1/hypotheses (142,7)"
    , "  o  m0/evt/INV/inv1/relation (142,7)"
    , "  o  m0/evt/INV/inv1/step (144,1)"
    , "  o  m0/evt/INV/inv1/step (146,1)"
    , "  o  m0/evt/INV/inv1/step (148,1)"
    , "  o  m0/evt/INV/inv1/step (150,1)"
    , "  o  m0/evt/INV/inv1/step (152,1)"
    , "  o  m0/evt/INV/inv1/step (154,1)"
    , "  o  m0/evt/INV/inv1/step (156,1)"
    , "  o  m0/evt/INV/inv2/easy (191,1)"
    , "  o  m0/evt/INV/inv3/goal (241,7)"
    , "  o  m0/evt/INV/inv3/hypotheses (241,7)"
    , "  o  m0/evt/INV/inv3/relation (241,7)"
    , "  o  m0/evt/INV/inv3/step (243,1)"
    , "  o  m0/evt/INV/inv3/step (245,1)"
    , "  o  m0/evt/INV/inv3/step (247,1)"
    , "  o  m0/evt/INV/inv3/step (253,1)"
    , "  o  m0/evt/INV/inv3/step (255,1)"
    , "  o  m0/evt/INV/inv4"
    , "  o  m0/evt/INV/inv5/assertion/asm0/easy (298,1)"
    , "  o  m0/evt/INV/inv5/main goal/goal (279,7)"
    , "  o  m0/evt/INV/inv5/main goal/hypotheses (279,7)"
    , "  o  m0/evt/INV/inv5/main goal/relation (279,7)"
    , "  o  m0/evt/INV/inv5/main goal/step (281,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (283,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (285,1)"
--     , " xxx m0/evt/INV/inv5/main goal/step (287,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (287,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (290,1)"
    -- , "  o  m0/evt/INV/inv6"
    , " xxx m0/evt/INV/inv6"
    , "  o  m0/evt/SCH"
    , "  o  m0/evt/WD/ACT/a0"
    , "  o  m0/evt/WD/ACT/a1"
    , "  o  m0/evt/WD/ACT/a2"
    , "  o  m0/evt/WD/ACT/a3"
    , "  o  m0/evt/WD/ACT/a4"
    , "  o  m0/evt/WD/C_SCH"
    , "  o  m0/evt/WD/F_SCH"
    , "  o  m0/evt/WD/GRD"
    , "passed 70 / 71"
    ]

path3 :: String
path3 = "tests/cubes-t3.tex"

result4 :: Either [Error] (Map Label ProgressProp)
result4 = either g Right (do
        q0 <- f `mzeq` zlambda [i_decl] 
            (mzle (mzint 0) i `mzand` mzless i bigN) 
            (mzpow i $ mzint 3)
        q1 <- bigN `mzeq` n
        q2 <- (k `mzless` n) `mzor` (n `mzeq` bigN)
        p1  <- (n `mzeq` k)
        p2  <- mzall [k `mzle` n, n `mzeq` k, mznot (n `mzeq` bigN)]
        p3 <-  mzall [n `mzeq` k, mznot (n `mzeq` bigN)]
        q3 <- mzor 
                (mzle k n `mzand` mznot (k `mzeq` n)) 
                (n `mzeq` bigN)
        q4 <- mznot (n `mzeq` k)
        return $ fromList 
            [   (label "prog0", LeadsTo [] ztrue q0)
            ,   (label "prog1", LeadsTo [] ztrue q1)
            ,   (label "prog2", LeadsTo [] p1 q2)
            ,   (label "prog3", LeadsTo [] p2 q3)
            ,   (label "prog4", LeadsTo [] p3 q4)
            ])
    where
        (k,_)      = var "k" int
        (i,i_decl) = var "i" int
        (f,_)      = var "f" (fun_type int int)
        (n,_)      = var "n" int
        (bigN,_)   = var "N" int
        g x = Left [Error x (LI path4 0 0)]

path4 :: String
path4 = "tests/cubes-t6.tex"

case4 :: IO (Either [Error] (Map Label ProgressProp))
case4 = runEitherT (do
    ms <- EitherT $ parse_machine path4 :: EitherT [Error] IO [Machine]
    case ms of
        [m] -> right $ progress $ props $ m
        _   -> left [Error "a single machine is expected" (LI "" 0 0)])

result5 :: Either [Error] (Map Label SafetyProp)
result5 = either g Right (do
        q0  <- bigN `mzeq` n
        p0  <- (k `mzle` n)
        q1  <- mznot (n `mzeq` k)
        p1  <- mzall
                [ n `mzeq` k
                , mznot (n `mzeq` bigN)
                ]
        return $ fromList 
            [   (label "saf0", Unless [k_decl] p0 q0 Nothing)
            ,   (label "saf1", Unless [k_decl] p1 q1 Nothing)
            ])
    where
        (k,k_decl) = var "k" int
        (n,_)      = var "n" int
        (bigN,_)   = var "N" int
        g x = Left [Error x (LI path4 0 0)]

case5 :: IO (Either [Error] (Map Label SafetyProp))
case5 = runEitherT (do
    ms <- EitherT $ parse_machine path4 :: EitherT [Error] IO [Machine]
    case ms of
        [m] -> right $ safety $ props $ m
        _   -> left [Error "a single machine is expected" (LI "" 0 0)])

case6 :: IO (String, Map Label Sequent)
case6 = verify path6

result6 :: String
result6 = unlines
    [ "  o  m0/INIT/FIS/a"
    , "  o  m0/INIT/FIS/b"
    , "  o  m0/INIT/FIS/c"
    , "  o  m0/INIT/FIS/f"
    , "  o  m0/INIT/FIS/n"
    , "  o  m0/INIT/INV/inv0"
    , "  o  m0/INIT/INV/inv1"
    , "  o  m0/INIT/INV/inv2"
    , "  o  m0/INIT/INV/inv3/goal (224,7)"
    , "  o  m0/INIT/INV/inv3/hypotheses (224,7)"
    , "  o  m0/INIT/INV/inv3/relation (224,7)"
    , "  o  m0/INIT/INV/inv3/step (226,1)"
    , "  o  m0/INIT/INV/inv3/step (228,1)"
    , "  o  m0/INIT/INV/inv3/step (232,1)"
    , "  o  m0/INIT/INV/inv4"
    , "  o  m0/INIT/INV/inv5"
    , "  o  m0/INIT/INV/inv6"
    , " xxx m0/INIT/INV/inv8"
    , "  o  m0/INIT/WD"
    , "  o  m0/INV/WD"
    , "  o  m0/evt/FIS/a@prime"
    , "  o  m0/evt/FIS/b@prime"
    , "  o  m0/evt/FIS/c@prime"
    , "  o  m0/evt/FIS/f@prime"
    , "  o  m0/evt/FIS/n@prime"
    , "  o  m0/evt/INV/inv0/goal (66,7)"
    , "  o  m0/evt/INV/inv0/hypotheses (66,7)"
    , "  o  m0/evt/INV/inv0/relation (66,7)"
    , "  o  m0/evt/INV/inv0/step (68,1)"
    , "  o  m0/evt/INV/inv0/step (70,1)"
    , "  o  m0/evt/INV/inv0/step (72,1)"
    , "  o  m0/evt/INV/inv0/step (74,1)"
    , "  o  m0/evt/INV/inv0/step (76,1)"
    , "  o  m0/evt/INV/inv1/goal (144,7)"
    , "  o  m0/evt/INV/inv1/hypotheses (144,7)"
    , "  o  m0/evt/INV/inv1/relation (144,7)"
    , "  o  m0/evt/INV/inv1/step (146,1)"
    , "  o  m0/evt/INV/inv1/step (148,1)"
    , "  o  m0/evt/INV/inv1/step (150,1)"
    , "  o  m0/evt/INV/inv1/step (152,1)"
    , "  o  m0/evt/INV/inv1/step (154,1)"
    , "  o  m0/evt/INV/inv1/step (156,1)"
    , "  o  m0/evt/INV/inv1/step (158,1)"
    , "  o  m0/evt/INV/inv2/easy (193,1)"
    , "  o  m0/evt/INV/inv3/goal (243,7)"
    , "  o  m0/evt/INV/inv3/hypotheses (243,7)"
    , "  o  m0/evt/INV/inv3/relation (243,7)"
    , "  o  m0/evt/INV/inv3/step (245,1)"
    , "  o  m0/evt/INV/inv3/step (247,1)"
    , "  o  m0/evt/INV/inv3/step (249,1)"
    , "  o  m0/evt/INV/inv3/step (255,1)"
    , "  o  m0/evt/INV/inv3/step (257,1)"
    , "  o  m0/evt/INV/inv4"
    , "  o  m0/evt/INV/inv5/assertion/asm0/easy (300,1)"
    , "  o  m0/evt/INV/inv5/main goal/goal (281,7)"
    , "  o  m0/evt/INV/inv5/main goal/hypotheses (281,7)"
    , "  o  m0/evt/INV/inv5/main goal/relation (281,7)"
    , "  o  m0/evt/INV/inv5/main goal/step (283,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (285,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (287,1)"
--     , " xxx m0/evt/INV/inv5/main goal/step (289,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (289,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (292,1)"
    , "  o  m0/evt/INV/inv6/goal (310,7)"
    , "  o  m0/evt/INV/inv6/hypotheses (310,7)"
    , "  o  m0/evt/INV/inv6/relation (310,7)"
    , "  o  m0/evt/INV/inv6/step (312,1)"
    , " xxx m0/evt/INV/inv6/step (314,1)"
    , "  o  m0/evt/INV/inv6/step (316,1)"
--     , " xxx m0/evt/INV/inv6/step (318,1)"
    , "  o  m0/evt/INV/inv6/step (318,1)"
    , " xxx m0/evt/INV/inv6/step (320,1)"
    , "  o  m0/evt/INV/inv8"
    , "  o  m0/evt/SAF/saf0"
    , "  o  m0/evt/SCH"
    , "  o  m0/evt/SCH/m0/1/REF/weaken"
    , "  o  m0/evt/TR/tr0/EN"
    , "  o  m0/evt/TR/tr0/NEG"
    , "  o  m0/evt/WD/ACT/a0"
    , "  o  m0/evt/WD/ACT/a1"
    , "  o  m0/evt/WD/ACT/a2"
    , "  o  m0/evt/WD/ACT/a3"
    , "  o  m0/evt/WD/ACT/a4"
    , "  o  m0/evt/WD/C_SCH"
    , "  o  m0/evt/WD/F_SCH"
    , "  o  m0/evt/WD/GRD"
    , "  o  m0/prog0/PROG/WD/lhs"
    , "  o  m0/prog0/PROG/WD/rhs"
    , "  o  m0/prog0/REF/monotonicity/lhs"
    , "  o  m0/prog0/REF/monotonicity/rhs"
    , "  o  m0/prog1/PROG/WD/lhs"
    , "  o  m0/prog1/PROG/WD/rhs"
    , " xxx m0/prog1/REF/add"
    , "  o  m0/prog2/PROG/WD/lhs"
    , "  o  m0/prog2/PROG/WD/rhs"
    , "  o  m0/prog2/REF/trading/lhs"
    , "  o  m0/prog2/REF/trading/rhs"
    , "  o  m0/prog3/PROG/WD/lhs"
    , "  o  m0/prog3/PROG/WD/rhs"
    , "  o  m0/prog3/REF/PSP/lhs"
    , "  o  m0/prog3/REF/PSP/rhs"
    , "  o  m0/prog4/PROG/WD/lhs"
    , "  o  m0/prog4/PROG/WD/rhs"
    , "  o  m0/prog4/REF/discharge/tr/lhs"
    , " xxx m0/prog4/REF/discharge/tr/rhs"
    , "  o  m0/saf0/SAF/WD/lhs"
    , "  o  m0/saf0/SAF/WD/rhs"
    , "  o  m0/tr0/TR/WD"
    , "passed 101 / 106"
    ]

path6 :: String
path6 = "tests/cubes-t5.tex"

case7 :: IO (String, Map Label Sequent)
case7 = verify path7

result7 :: String
result7 = unlines
    [ "  o  m0/INIT/FIS/a"
    , "  o  m0/INIT/FIS/b"
    , "  o  m0/INIT/FIS/c"
    , "  o  m0/INIT/FIS/f"
    , "  o  m0/INIT/FIS/n"
    , "  o  m0/INIT/INV/inv0"
    , "  o  m0/INIT/INV/inv1"
    , "  o  m0/INIT/INV/inv2"
    , "  o  m0/INIT/INV/inv3/goal (224,7)"
    , "  o  m0/INIT/INV/inv3/hypotheses (224,7)"
    , "  o  m0/INIT/INV/inv3/relation (224,7)"
    , "  o  m0/INIT/INV/inv3/step (226,1)"
    , "  o  m0/INIT/INV/inv3/step (228,1)"
    , "  o  m0/INIT/INV/inv3/step (232,1)"
    , "  o  m0/INIT/INV/inv4"
    , "  o  m0/INIT/INV/inv5"
    , "  o  m0/INIT/INV/inv6"
    , "  o  m0/INIT/WD"
    , "  o  m0/INV/WD"
    , "  o  m0/evt/FIS/a@prime"
    , "  o  m0/evt/FIS/b@prime"
    , "  o  m0/evt/FIS/c@prime"
    , "  o  m0/evt/FIS/f@prime"
    , "  o  m0/evt/FIS/n@prime"
    , "  o  m0/evt/INV/inv0/goal (66,7)"
    , "  o  m0/evt/INV/inv0/hypotheses (66,7)"
    , "  o  m0/evt/INV/inv0/relation (66,7)"
    , "  o  m0/evt/INV/inv0/step (68,1)"
    , "  o  m0/evt/INV/inv0/step (70,1)"
    , "  o  m0/evt/INV/inv0/step (72,1)"
    , "  o  m0/evt/INV/inv0/step (74,1)"
    , "  o  m0/evt/INV/inv0/step (76,1)"
    , "  o  m0/evt/INV/inv1/goal (144,7)"
    , "  o  m0/evt/INV/inv1/hypotheses (144,7)"
    , "  o  m0/evt/INV/inv1/relation (144,7)"
    , "  o  m0/evt/INV/inv1/step (146,1)"
    , "  o  m0/evt/INV/inv1/step (148,1)"
    , "  o  m0/evt/INV/inv1/step (150,1)"
    , "  o  m0/evt/INV/inv1/step (152,1)"
    , "  o  m0/evt/INV/inv1/step (154,1)"
    , "  o  m0/evt/INV/inv1/step (156,1)"
    , "  o  m0/evt/INV/inv1/step (158,1)"
    , "  o  m0/evt/INV/inv2/easy (193,1)"
    , "  o  m0/evt/INV/inv3/goal (243,7)"
    , "  o  m0/evt/INV/inv3/hypotheses (243,7)"
    , "  o  m0/evt/INV/inv3/relation (243,7)"
    , "  o  m0/evt/INV/inv3/step (245,1)"
    , "  o  m0/evt/INV/inv3/step (247,1)"
    , "  o  m0/evt/INV/inv3/step (249,1)"
    , "  o  m0/evt/INV/inv3/step (255,1)"
    , "  o  m0/evt/INV/inv3/step (257,1)"
    , "  o  m0/evt/INV/inv4"
    , "  o  m0/evt/INV/inv5/assertion/asm0/easy (300,1)"
    , "  o  m0/evt/INV/inv5/main goal/goal (281,7)"
    , "  o  m0/evt/INV/inv5/main goal/hypotheses (281,7)"
    , "  o  m0/evt/INV/inv5/main goal/relation (281,7)"
    , "  o  m0/evt/INV/inv5/main goal/step (283,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (285,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (287,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (289,1)"
--     , " xxx m0/evt/INV/inv5/main goal/step (289,1)"
    , "  o  m0/evt/INV/inv5/main goal/step (292,1)"
    , "  o  m0/evt/INV/inv6/goal (310,7)"
    , "  o  m0/evt/INV/inv6/hypotheses (310,7)"
    , "  o  m0/evt/INV/inv6/relation (310,7)"
    , "  o  m0/evt/INV/inv6/step (312,1)"
    , " xxx m0/evt/INV/inv6/step (314,1)"
    , "  o  m0/evt/INV/inv6/step (316,1)"
--     , " xxx m0/evt/INV/inv6/step (318,1)"
    , "  o  m0/evt/INV/inv6/step (318,1)"
    , " xxx m0/evt/INV/inv6/step (320,1)"
    , "  o  m0/evt/SAF/saf0"
    , "  o  m0/evt/SCH"
    , "  o  m0/evt/WD/ACT/a0"
    , "  o  m0/evt/WD/ACT/a1"
    , "  o  m0/evt/WD/ACT/a2"
    , "  o  m0/evt/WD/ACT/a3"
    , "  o  m0/evt/WD/ACT/a4"
    , "  o  m0/evt/WD/C_SCH"
    , "  o  m0/evt/WD/F_SCH"
    , "  o  m0/evt/WD/GRD"
    , "  o  m0/prog0/PROG/WD/lhs"
    , "  o  m0/prog0/PROG/WD/rhs"
    , "  o  m0/prog0/REF/monotonicity/lhs"
    , "  o  m0/prog0/REF/monotonicity/rhs"
    , "  o  m0/prog1/PROG/WD/lhs"
    , "  o  m0/prog1/PROG/WD/rhs"
    , " xxx m0/prog1/REF/add"
    , "  o  m0/prog10/PROG/WD/lhs"
    , "  o  m0/prog10/PROG/WD/rhs"
    , " xxx m0/prog10/REF/add"
    , "  o  m0/prog2/PROG/WD/lhs"
    , "  o  m0/prog2/PROG/WD/rhs"
    , " xxx m0/prog2/REF/trading/lhs"
    , "  o  m0/prog2/REF/trading/rhs"
    , "  o  m0/prog3/PROG/WD/lhs"
    , "  o  m0/prog3/PROG/WD/rhs"
    , " xxx m0/prog3/REF/PSP/lhs"
    , " xxx m0/prog3/REF/PSP/rhs"
    , "  o  m0/prog4/PROG/WD/lhs"
    , "  o  m0/prog4/PROG/WD/rhs"
    , " xxx m0/prog4/REF/add"
    , "  o  m0/prog5/PROG/WD/lhs"
    , "  o  m0/prog5/PROG/WD/rhs"
    , " xxx m0/prog5/REF/transitivity/lhs"
    , "  o  m0/prog5/REF/transitivity/mhs"
    , "  o  m0/prog5/REF/transitivity/rhs"
    , "  o  m0/prog6/PROG/WD/lhs"
    , "  o  m0/prog6/PROG/WD/rhs"
    , " xxx m0/prog6/REF/add"
    , "  o  m0/prog7/PROG/WD/lhs"
    , "  o  m0/prog7/PROG/WD/rhs"
    , " xxx m0/prog7/REF/add"
    , "  o  m0/prog8/PROG/WD/lhs"
    , "  o  m0/prog8/PROG/WD/rhs"
    , "  o  m0/prog8/REF/transitivity/lhs"
    , "  o  m0/prog8/REF/transitivity/mhs"
    , "  o  m0/prog8/REF/transitivity/rhs"
    , "  o  m0/prog9/PROG/WD/lhs"
    , "  o  m0/prog9/PROG/WD/rhs"
    , " xxx m0/prog9/REF/add"
    , "  o  m0/saf0/SAF/WD/lhs"
    , "  o  m0/saf0/SAF/WD/rhs"
    , "passed 109 / 121"
    ]

path7 :: String
path7 = "tests/cubes-t4.tex"

case8 :: IO (String, Map Label Sequent)
case8 = verify path8

result8 :: String
result8 = unlines
    [ "  o  m0/INIT/FIS/a"
    , "  o  m0/INIT/FIS/b"
    , "  o  m0/INIT/FIS/c"
    , "  o  m0/INIT/FIS/f"
    , "  o  m0/INIT/FIS/n"
    , "  o  m0/INIT/INV/inv0"
    , "  o  m0/INIT/INV/inv1"
    , "  o  m0/INIT/INV/inv2"
    , "  o  m0/INIT/INV/inv3/goal (224,7)"
    , "  o  m0/INIT/INV/inv3/hypotheses (224,7)"
    , "  o  m0/INIT/INV/inv3/relation (224,7)"
    , "  o  m0/INIT/INV/inv3/step (226,1)"
    , "  o  m0/INIT/INV/inv3/step (228,1)"
    , "  o  m0/INIT/INV/inv3/step (232,1)"
    , "  o  m0/INIT/INV/inv4"
    , "  o  m0/INIT/INV/inv5"
    , "  o  m0/INIT/INV/inv6"
    , "  o  m0/INIT/INV/inv7"
    , "  o  m0/INIT/WD"
    , "  o  m0/INV/WD"
    , "  o  m0/evt/FIS/a@prime"
    , "  o  m0/evt/FIS/b@prime"
    , "  o  m0/evt/FIS/c@prime"
    , "  o  m0/evt/FIS/f@prime"
    , "  o  m0/evt/FIS/n@prime"
    , "  o  m0/evt/INV/inv0/goal (66,7)"
    , "  o  m0/evt/INV/inv0/hypotheses (66,7)"
    , "  o  m0/evt/INV/inv0/relation (66,7)"
    , "  o  m0/evt/INV/inv0/step (68,1)"
    , "  o  m0/evt/INV/inv0/step (70,1)"
    , "  o  m0/evt/INV/inv0/step (72,1)"
    , "  o  m0/evt/INV/inv0/step (74,1)"
    , "  o  m0/evt/INV/inv0/step (76,1)"
    , "  o  m0/evt/INV/inv1/goal (144,7)"
    , "  o  m0/evt/INV/inv1/hypotheses (144,7)"
    , "  o  m0/evt/INV/inv1/relation (144,7)"
    , "  o  m0/evt/INV/inv1/step (146,1)"
    , "  o  m0/evt/INV/inv1/step (148,1)"
    , "  o  m0/evt/INV/inv1/step (150,1)"
    , "  o  m0/evt/INV/inv1/step (152,1)"
    , "  o  m0/evt/INV/inv1/step (154,1)"
    , "  o  m0/evt/INV/inv1/step (156,1)"
    , "  o  m0/evt/INV/inv1/step (158,1)"
    , "  o  m0/evt/INV/inv2/easy (193,1)"
    , "  o  m0/evt/INV/inv3/goal (243,7)"
    , "  o  m0/evt/INV/inv3/hypotheses (243,7)"
    , "  o  m0/evt/INV/inv3/relation (243,7)"
    , "  o  m0/evt/INV/inv3/step (245,1)"
    , "  o  m0/evt/INV/inv3/step (247,1)"
    , "  o  m0/evt/INV/inv3/step (249,1)"
    , "  o  m0/evt/INV/inv3/step (255,1)"
    , "  o  m0/evt/INV/inv3/step (257,1)"
    , "  o  m0/evt/INV/inv4"
--     , "  o  m0/evt/INV/inv5/assertion/asm0/easy (300,1)"
    , "  o  m0/evt/INV/inv5/goal (281,7)"
    , "  o  m0/evt/INV/inv5/hypotheses (281,7)"
    , "  o  m0/evt/INV/inv5/relation (281,7)"
    , "  o  m0/evt/INV/inv5/step (283,1)"
    , "  o  m0/evt/INV/inv5/step (285,1)"
    , "  o  m0/evt/INV/inv5/step (287,1)"
    , "  o  m0/evt/INV/inv5/step (289,1)"
--     , " xxx m0/evt/INV/inv5/step (289,1)"
    , "  o  m0/evt/INV/inv5/step (292,1)"
    , "  o  m0/evt/INV/inv6/goal (310,7)"
    , "  o  m0/evt/INV/inv6/hypotheses (310,7)"
    , "  o  m0/evt/INV/inv6/relation (310,7)"
    , "  o  m0/evt/INV/inv6/step (312,1)"
    , " xxx m0/evt/INV/inv6/step (314,1)"
    , "  o  m0/evt/INV/inv6/step (316,1)"
    , "  o  m0/evt/INV/inv6/step (318,1)"
    , " xxx m0/evt/INV/inv6/step (320,1)"
    , "  o  m0/evt/INV/inv7"
    , "  o  m0/evt/SAF/saf0"
    , "  o  m0/evt/SAF/saf1"
    , "  o  m0/evt/SCH"
    , "  o  m0/evt/SCH/m0/1/REF/weaken"
    , "  o  m0/evt/TR/tr0/EN"
    , "  o  m0/evt/TR/tr0/NEG"
    , "  o  m0/evt/WD/ACT/a0"
    , "  o  m0/evt/WD/ACT/a1"
    , "  o  m0/evt/WD/ACT/a2"
    , "  o  m0/evt/WD/ACT/a3"
    , "  o  m0/evt/WD/ACT/a4"
    , "  o  m0/evt/WD/C_SCH"
    , "  o  m0/evt/WD/F_SCH"
    , "  o  m0/evt/WD/GRD"
    , "  o  m0/prog0/PROG/WD/lhs"
    , "  o  m0/prog0/PROG/WD/rhs"
    , "  o  m0/prog0/REF/monotonicity/lhs"
    , "  o  m0/prog0/REF/monotonicity/rhs"
    , "  o  m0/prog1/PROG/WD/lhs"
    , "  o  m0/prog1/PROG/WD/rhs"
    , "  o  m0/prog1/REF/induction/lhs"
    , "  o  m0/prog1/REF/induction/rhs"
    , "  o  m0/prog2/PROG/WD/lhs"
    , "  o  m0/prog2/PROG/WD/rhs"
    , "  o  m0/prog2/REF/trading/lhs"
    , "  o  m0/prog2/REF/trading/rhs"
    , "  o  m0/prog3/PROG/WD/lhs"
    , "  o  m0/prog3/PROG/WD/rhs"
    , "  o  m0/prog3/REF/PSP/lhs"
    , "  o  m0/prog3/REF/PSP/rhs"
    , "  o  m0/prog4/PROG/WD/lhs"
    , "  o  m0/prog4/PROG/WD/rhs"
    , "  o  m0/prog4/REF/discharge/saf/lhs"
    , "  o  m0/prog4/REF/discharge/saf/rhs"
    , "  o  m0/prog4/REF/discharge/tr"
    , "  o  m0/saf0/SAF/WD/lhs"
    , "  o  m0/saf0/SAF/WD/rhs"
    , "  o  m0/saf1/SAF/WD/lhs"
    , "  o  m0/saf1/SAF/WD/rhs"
    , "  o  m0/tr0/TR/WD"
    , "passed 108 / 110"
    ]
  
path8 :: String
path8 = "tests/cubes-t7.tex"

result9 :: String
result9 = unlines
    [ "  o  m0/INIT/FIS/a"
    , "  o  m0/INIT/FIS/b"
    , "  o  m0/INIT/FIS/c"
    , "  o  m0/INIT/FIS/f"
    , "  o  m0/INIT/FIS/n"
    , "  o  m0/INIT/INV/inv0"
    , "  o  m0/INIT/INV/inv1"
    , "  o  m0/INIT/INV/inv2"
    , "  o  m0/INIT/INV/inv3/goal (224,7)"
    , "  o  m0/INIT/INV/inv3/hypotheses (224,7)"
    , "  o  m0/INIT/INV/inv3/relation (224,7)"
    , "  o  m0/INIT/INV/inv3/step (226,1)"
    , "  o  m0/INIT/INV/inv3/step (228,1)"
    , "  o  m0/INIT/INV/inv3/step (232,1)"
    , "  o  m0/INIT/INV/inv4"
    , "  o  m0/INIT/INV/inv5"
    , "  o  m0/INIT/INV/inv6"
    , "  o  m0/INIT/INV/inv7"
    , "  o  m0/INIT/WD"
    , "  o  m0/INV/WD"
    , "  o  m0/evt/FIS/a@prime"
    , "  o  m0/evt/FIS/b@prime"
    , "  o  m0/evt/FIS/c@prime"
    , "  o  m0/evt/FIS/f@prime"
    , "  o  m0/evt/FIS/n@prime"
    , "  o  m0/evt/INV/inv0/goal (66,7)"
    , "  o  m0/evt/INV/inv0/hypotheses (66,7)"
    , "  o  m0/evt/INV/inv0/relation (66,7)"
    , "  o  m0/evt/INV/inv0/step (68,1)"
    , "  o  m0/evt/INV/inv0/step (70,1)"
    , "  o  m0/evt/INV/inv0/step (72,1)"
    , "  o  m0/evt/INV/inv0/step (74,1)"
    , "  o  m0/evt/INV/inv0/step (76,1)"
    , "  o  m0/evt/INV/inv1/goal (144,7)"
    , "  o  m0/evt/INV/inv1/hypotheses (144,7)"
    , "  o  m0/evt/INV/inv1/relation (144,7)"
    , "  o  m0/evt/INV/inv1/step (146,1)"
    , "  o  m0/evt/INV/inv1/step (148,1)"
    , "  o  m0/evt/INV/inv1/step (150,1)"
    , "  o  m0/evt/INV/inv1/step (152,1)"
    , "  o  m0/evt/INV/inv1/step (154,1)"
    , "  o  m0/evt/INV/inv1/step (156,1)"
    , "  o  m0/evt/INV/inv1/step (158,1)"
    , "  o  m0/evt/INV/inv2/easy (193,1)"
    , "  o  m0/evt/INV/inv3/goal (243,7)"
    , "  o  m0/evt/INV/inv3/hypotheses (243,7)"
    , "  o  m0/evt/INV/inv3/relation (243,7)"
    , "  o  m0/evt/INV/inv3/step (245,1)"
    , "  o  m0/evt/INV/inv3/step (247,1)"
    , "  o  m0/evt/INV/inv3/step (249,1)"
    , "  o  m0/evt/INV/inv3/step (255,1)"
    , "  o  m0/evt/INV/inv3/step (257,1)"
    , "  o  m0/evt/INV/inv4"
    , "  o  m0/evt/INV/inv5/goal (281,7)"
    , "  o  m0/evt/INV/inv5/hypotheses (281,7)"
    , "  o  m0/evt/INV/inv5/relation (281,7)"
    , "  o  m0/evt/INV/inv5/step (283,1)"
    , "  o  m0/evt/INV/inv5/step (285,1)"
    , "  o  m0/evt/INV/inv5/step (287,1)"
    , "  o  m0/evt/INV/inv5/step (289,1)"
    , "  o  m0/evt/INV/inv5/step (292,1)"
    , "  o  m0/evt/INV/inv6/goal (310,7)"
    , "  o  m0/evt/INV/inv6/hypotheses (310,7)"
    , "  o  m0/evt/INV/inv6/relation (310,7)"
    , "  o  m0/evt/INV/inv6/step (312,1)"
    , " xxx m0/evt/INV/inv6/step (314,1)"
    , "  o  m0/evt/INV/inv6/step (316,1)"
    , "  o  m0/evt/INV/inv6/step (318,1)"
    , " xxx m0/evt/INV/inv6/step (320,1)"
    , "  o  m0/evt/INV/inv7"
    , "  o  m0/evt/SAF/saf0"
    , "  o  m0/evt/SAF/saf1"
    , "  o  m0/evt/SCH"
    , "  o  m0/evt/SCH/m0/1/REF/weaken"
    , "  o  m0/evt/TR/tr0/EN"
    , "  o  m0/evt/TR/tr0/NEG"
    , "  o  m0/evt/WD/ACT/a0"
    , "  o  m0/evt/WD/ACT/a1"
    , "  o  m0/evt/WD/ACT/a2"
    , "  o  m0/evt/WD/ACT/a3"
    , "  o  m0/evt/WD/ACT/a4"
    , "  o  m0/evt/WD/C_SCH"
    , "  o  m0/evt/WD/F_SCH"
    , "  o  m0/evt/WD/GRD"
    , "  o  m0/prog0/PROG/WD/lhs"
    , "  o  m0/prog0/PROG/WD/rhs"
    , "  o  m0/prog0/REF/monotonicity/lhs"
    , "  o  m0/prog0/REF/monotonicity/rhs"
    , "  o  m0/prog1/PROG/WD/lhs"
    , "  o  m0/prog1/PROG/WD/rhs"
    , "  o  m0/prog1/REF/induction/lhs"
    , "  o  m0/prog1/REF/induction/rhs"
    , "  o  m0/prog2/PROG/WD/lhs"
    , "  o  m0/prog2/PROG/WD/rhs"
    , "  o  m0/prog2/REF/trading/lhs"
    , "  o  m0/prog2/REF/trading/rhs"
    , "  o  m0/prog3/PROG/WD/lhs"
    , "  o  m0/prog3/PROG/WD/rhs"
    , "  o  m0/prog3/REF/PSP/lhs"
    , "  o  m0/prog3/REF/PSP/rhs"
    , "  o  m0/prog4/PROG/WD/lhs"
    , "  o  m0/prog4/PROG/WD/rhs"
    , "  o  m0/prog4/REF/discharge/saf/lhs"
    , "  o  m0/prog4/REF/discharge/saf/rhs"
    , "  o  m0/prog4/REF/discharge/tr"
    , "  o  m0/prog5/PROG/WD/lhs"
    , "  o  m0/prog5/PROG/WD/rhs"
    , "  o  m0/prog5/REF/disjunction/lhs"
    , "  o  m0/prog5/REF/disjunction/rhs"
    , "  o  m0/prog6/PROG/WD/lhs"
    , "  o  m0/prog6/PROG/WD/rhs"
    , " xxx m0/prog6/REF/add"
    , "  o  m0/prog7/PROG/WD/lhs"
    , "  o  m0/prog7/PROG/WD/rhs"
    , " xxx m0/prog7/REF/add"
    , "  o  m0/prog8/PROG/WD/lhs"
    , "  o  m0/prog8/PROG/WD/rhs"
    , " xxx m0/prog8/REF/add"
    , "  o  m0/saf0/SAF/WD/lhs"
    , "  o  m0/saf0/SAF/WD/rhs"
    , "  o  m0/saf1/SAF/WD/lhs"
    , "  o  m0/saf1/SAF/WD/rhs"
    , "  o  m0/tr0/TR/WD"
    , "passed 118 / 123"
    ]

path9 :: String
path9 = "tests/cubes-t8.tex"
     
path10 :: String
path10 = "tests/cubes-t9.tex"

result10 :: String
result10 = "Left [Error \"A cycle exists in the proof of liveness: \
            \prog0, prog1, prog2, prog3\" (1,1)]"

case11 :: IO String
case11 = do
        pos <- list_file_obligations path2
        case pos of
            Right [(_,pos)] -> do
                let po = pos ! label "m0/evt/INV/inv5/main goal/step (287,1)"
                let cmd = concatMap pretty_print' $ z3_code po
                return cmd
            x -> return $ show x

result11 :: String
result11 = unlines 
    [ "(declare-datatypes (a) ( (Maybe (Just (fromJust a)) Nothing) ))"
    , "(declare-datatypes () ( (Null null) ))"
    , "(declare-datatypes (a b) ( (Pair (pair (first a) (second b))) ))"
    , "; comment: we don't need to declare the sort Bool"
    , "; comment: we don't need to declare the sort Int"
    , "; comment: we don't need to declare the sort Real"
    , "(define-sort pfun (a b) (Array a (Maybe b)))"
    , "(define-sort set (a) (Array a Bool))"
    , "(declare-const a Int)"
    , "(declare-const a@prime Int)"
    , "(declare-const b Int)"
    , "(declare-const b@prime Int)"
    , "(declare-const c Int)"
    , "(declare-const c@prime Int)"
    , "(declare-const f (pfun Int Int))"
    , "(declare-const f@prime (pfun Int Int))"
    , "(declare-const i Int)"
    , "(declare-const n Int)"
    , "(declare-const n@prime Int)"
    , "(declare-fun apply@@Int@@Int ( (pfun Int Int)   Int ) Int)"
    , "(declare-fun bunion@@Int ( (set Int)   (set Int) ) (set Int))"
    , "(declare-fun dom-rest@@Int@@Int"
    , "             ( (set Int)"
    , "               (pfun Int Int) )"
    , "             (pfun Int Int))"
    , "(declare-fun dom-subt@@Int@@Int"
    , "             ( (set Int)"
    , "               (pfun Int Int) )"
    , "             (pfun Int Int))"
    , "(declare-fun dom@@Int@@Int ( (pfun Int Int) ) (set Int))"
    , "(declare-fun elem@@Int (Int (set Int)) Bool)"
    , "(declare-fun empty-fun@@Int@@Int () (pfun Int Int))"
    , "(declare-fun empty-set@@Int () (set Int))"
    , "(declare-fun intersect@@Int"
    , "             ( (set Int)"
    , "               (set Int) )"
    , "             (set Int))"
    , "(declare-fun mk-fun@@Int@@Int (Int Int) (pfun Int Int))"
    , "(declare-fun mk-set@@Int (Int) (set Int))"
    , "(declare-fun ovl@@Int@@Int"
    , "             ( (pfun Int Int)"
    , "               (pfun Int Int) )"
    , "             (pfun Int Int))"
    , "(declare-fun ran@@Int@@Int ( (pfun Int Int) ) (set Int))"
    , "(declare-fun set-diff@@Int ( (set Int)   (set Int) ) (set Int))"
    , "(declare-fun set@@Int@@Int ( (pfun Int Int) ) (set Int))"
    , "(declare-fun subset@@Int ( (set Int)   (set Int) ) Bool)"
    , "(assert (forall ( (i Int) )"
    , "                (=> true"
    , "                    (= (elem@@Int i (dom@@Int@@Int f))"
    , "                       (and (<= 0 i) (< i n))))))"
    , "(assert (= f@prime (ovl@@Int@@Int f (mk-fun@@Int@@Int n a))))"
    , "(assert (forall ( (f1 (pfun Int Int))"
    , "                  (f2 (pfun Int Int)) )"
    , "                (=> true"
    , "                    (= (bunion@@Int (dom@@Int@@Int f1) \
                                          \(dom@@Int@@Int f2))"
    , "                       (dom@@Int@@Int (ovl@@Int@@Int f1 f2))))))"
    , "(assert (= (dom@@Int@@Int empty-fun@@Int@@Int)"
    , "           empty-set@@Int))"
    , "(assert (forall ( (x Int)"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (dom@@Int@@Int (mk-fun@@Int@@Int x y))"
    , "                       (mk-set@@Int x)))))"
    , "(assert (forall ( (f1 (pfun Int Int))"
    , "                  (f2 (pfun Int Int))"
    , "                  (x Int) )"
    , "                (=> true"
    , "                    (=> (elem@@Int x (dom@@Int@@Int f2))"
    , "                        (= (apply@@Int@@Int (ovl@@Int@@Int f1 f2) x)"
    , "                           (apply@@Int@@Int f2 x))))))"
    , "(assert (forall ( (f1 (pfun Int Int))"
    , "                  (f2 (pfun Int Int))"
    , "                  (x Int) )"
    , "                (=> true"
    , "                    (=> (and (elem@@Int x (dom@@Int@@Int f1))"
    , "                             (not (elem@@Int x (dom@@Int@@Int f2))))"
    , "                        (= (apply@@Int@@Int (ovl@@Int@@Int f1 f2) x)"
    , "                           (apply@@Int@@Int f1 x))))))"
    , "(assert (forall ( (f1 (pfun Int Int))"
    , "                  (s1 (set Int)) )"
    , "                (=> true"
    , "                    (= (dom@@Int@@Int (dom-subt@@Int@@Int s1 f1))"
    , "                       (set-diff@@Int (dom@@Int@@Int f1) s1)))))"
    , "(assert (forall ( (x Int)"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (apply@@Int@@Int (mk-fun@@Int@@Int x y) x) y))))"
    , "(assert (forall ( (f1 (pfun Int Int))"
    , "                  (s1 (set Int))"
    , "                  (x Int) )"
    , "                (=> true"
    , "                    (=> (and (elem@@Int x s1) \
                                   \(elem@@Int x (dom@@Int@@Int f1)))"
    , "                        (= (apply@@Int@@Int (dom-rest@@Int@@Int s1 f1)\
                                                  \ x)"
    , "                           (apply@@Int@@Int f1 x))))))"
    , "(assert (forall ( (f1 (pfun Int Int))"
    , "                  (s1 (set Int))"
    , "                  (x Int) )"
    , "                (=> true"
    , "                    (=> (elem@@Int x\
                                       \ (set-diff@@Int (dom@@Int@@Int f1)\
                                                       \ s1))"
    , "                        (= (apply@@Int@@Int (dom-subt@@Int@@Int s1 f1)\
                                                  \ x)"
    , "                           (apply@@Int@@Int f1 x))))))"
    , "(assert (forall ( (x Int) )"
    , "                (=> true"
    , "                    (= (select empty-fun@@Int@@Int x)"
    , "                       (as Nothing (Maybe Int))))))"
    , "(assert (forall ( (x Int)"
    , "                  (x2 Int)"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (select (mk-fun@@Int@@Int x y) x2)"
    , "                       (ite (= x x2)\
                                 \ (Just y)\
                                 \ (as Nothing (Maybe Int)))))))"
    , "(assert (forall ( (x Int)"
    , "                  (f1 (pfun Int Int))"
    , "                  (f2 (pfun Int Int)) )"
    , "                (=> true"
    , "                    (= (select (ovl@@Int@@Int f1 f2) x)"
    , "                       (ite (= (select f2 x) (as Nothing (Maybe Int)))"
    , "                            (select f1 x)"
    , "                            (select f2 x))))))"
    , "(assert (forall ( (x Int)"
    , "                  (f1 (pfun Int Int)) )"
    , "                (=> true"
    , "                    (= (select (dom@@Int@@Int f1) x)"
    , "                       (not (= (select f1 x)\
                                    \ (as Nothing (Maybe Int))))))))"
    , "(assert (forall ( (y Int)"
    , "                  (f1 (pfun Int Int)) )"
    , "                (=> true"
    , "                    (= (elem@@Int y (set@@Int@@Int f1))"
    , "                       (exists ( (x Int) )"
    , "                               (and (elem@@Int x (dom@@Int@@Int f1))"
    , "                                    (= (apply@@Int@@Int f1 x) y)))))))"
    , "(assert (forall ( (x Int)"
    , "                  (y Int)"
    , "                  (f1 (pfun Int Int)) )"
    , "                (=> true"
    , "                    (= (and (elem@@Int x (dom@@Int@@Int f1))"
    , "                            (= (apply@@Int@@Int f1 x) y))"
    , "                       (= (select f1 x) (Just y))))))"
    , "(assert (forall ( (f1 (pfun Int Int))"
    , "                  (x2 Int)"
    , "                  (x Int)"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (=> (not (= x x2))"
    , "                        (= (apply@@Int@@Int\
                                  \ (ovl@@Int@@Int\
                                     \ f1\
                                     \ (mk-fun@@Int@@Int x y))\
                                  \ x2)"
    , "                           (apply@@Int@@Int f1 x2))))))"
    , "(assert (forall ( (f1 (pfun Int Int))"
    , "                  (x Int)"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (apply@@Int@@Int\
                              \ (ovl@@Int@@Int f1 (mk-fun@@Int@@Int x y))\
                              \ x)"
    , "                       y))))"
    , "(assert (= (ran@@Int@@Int empty-fun@@Int@@Int)"
    , "           empty-set@@Int))"
    , "(assert (forall ( (f1 (pfun Int Int))"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (elem@@Int y (ran@@Int@@Int f1))"
    , "                       (exists ( (x Int) )"
    , "                               (and true"
    , "                                    (and (elem@@Int\
                                                \ x\
                                                \ (dom@@Int@@Int f1))"
    , "                                         (= (apply@@Int@@Int f1 x)\
                                                 \ y))))))))"
    , "(assert (forall ( (x Int)"
    , "                  (y Int) )"
    , "                (=> true"
    , "                    (= (ran@@Int@@Int (mk-fun@@Int@@Int x y))"
    , "                       (mk-set@@Int y)))))"
    , "(assert (forall ( (f1 (pfun Int Int))"
    , "                  (f2 (pfun Int Int)) )"
    , "                (=> true"
    , "                    (subset@@Int (ran@@Int@@Int (ovl@@Int@@Int f1 f2))"
    , "                                 (bunion@@Int (ran@@Int@@Int f1)\
                                                   \ (ran@@Int@@Int f2))))))"
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
    , "                               (=> true (=> (elem@@Int x s1)\
                                                 \ (elem@@Int x s2))))))))"
    , "(assert (forall ( (s1 (set Int))"
    , "                  (s2 (set Int)) )"
    , "                (=> true"
    , "                    (= (and (subset@@Int s1 s2) (subset@@Int s2 s1))"
    , "                       (= s1 s2)))))"
    , "(assert (forall ( (i Int) )"
    , "                (=> true"
    , "                    (= (elem@@Int i (dom@@Int@@Int f))"
    , "                       (and (<= 0 i) (< i n))))))"
--    , "; ~goal0"
    , "(assert (not (= (forall ( (i Int) )"
    , "                        (=> (and (<= 0 i) (< i n))"
    , "                            (= (apply@@Int@@Int f@prime i) (^ i 3))))"
    , "                (forall ( (i Int) )"
    , "                        (=> (and (<= 0 i) (< i n))"
    , "                            (= (apply@@Int@@Int f i) (^ i 3)))))))"
--    , "; ~goal1"
    , "(assert (not (forall ( (i Int) )"
    , "                     (=> true"
    , "                         (= (=> (and (<= 0 i) (< i n))"
    , "                                (= (apply@@Int@@Int f@prime i) (^ i 3)))"
    , "                            (=> (and (<= 0 i) (< i n))"
    , "                                (= (apply@@Int@@Int f i) (^ i 3))))))))"
    , "(assert (not (= (=> (and (<= 0 i) (< i n))"
    , "                    (= (apply@@Int@@Int f@prime i) (^ i 3)))"
    , "                (=> (and (<= 0 i) (< i n))"
    , "                    (= (apply@@Int@@Int f i) (^ i 3))))))"
--    , "; ~goal2"
    , "(assert (not (= (= (apply@@Int@@Int f@prime i) (^ i 3))"
    , "                (= (apply@@Int@@Int f i) (^ i 3)))))"
--    , "; ~goal3"
    , "(assert (not (= (apply@@Int@@Int f@prime i) (apply@@Int@@Int f i))))"
--    , "; ~goal4"
    , "(assert (not (= f@prime f)))"
    , "(check-sat-using (or-else (then qe smt)"
    , "                          (then simplify smt)"
    , "                          (then skip smt)"
    , "                          (then (using-params\
                                      \ simplify\
                                      \ :expand-power\
                                      \ true)\
                                     \ smt)))"
    ]

path12 :: String
path12 = "Tests/cubes-t10.tex"

result12 :: String
result12 = unlines 
        [  "error (274,2): type of j is ill-defined: _a"
        ]

case12 :: IO String
case12 = do
        r <- parse_machine path12
        case r of
            Right _ -> do
                return "successful verification"
            Left xs -> return $ unlines $ map format_error xs


verify :: FilePath -> IO (String, Map Label Sequent)
verify path = do
    r <- list_file_obligations path
    case r of
        Right [(m,pos)] -> do
            (s,_,_) <- str_verify_machine m
            return (s, pos)
        x -> return (show x, empty)