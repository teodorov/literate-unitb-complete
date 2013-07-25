module Z3.Test where

--import Data.Maybe

import Tests.UnitTest

import Z3.Z3 as Z

import UnitB.Calculation
import UnitB.Operator
import UnitB.PO
import UnitB.AST

cases = test_cases [case0, case1, case2, case3, case4]

test_case = ("Z3 test", cases, True)

case0 = Case "sample_quant" (verify sample_quant) $ Right Sat
case1 = Case "sample_quant2" (verify sample_quant2) $ Right Sat
case2 = Case "sample_quant3" (verify sample_quant3) $ Right Unsat
case3 = Case "sample proof" (discharge sample_proof) Valid

case4 = Case "check sample calc" (check empty_theory sample_calc) (Right [])


sample = unlines [
    "(declare-const a Int)",
    "(declare-fun f (Int Bool) Int)",
    "",
    "(assert (> a 10))",
    "(assert (< (f a true) 100))",
    "(check-sat)",
    "(assert (< a 10))",
    "(check-sat)",
    "(get-model)"]

a        = Word (Var "a" int)
(x,x')   = var "x" int
ff       = Fun [] "f" [int, BOOL] int

sample_ast = [
        Decl (ConstDecl "a" int),
        Decl (FunDecl [] "f" [int, BOOL] int),
        Assert (a `zgreater` zint 10),
        Assert (f a ztrue `zless` zint 10),
        CheckSat False ]
    where
        f       = fun2 ff

sample_quant = [
        Decl (FunDecl [] "f" [int] int),
        Assert $ fromJust (mzforall [x'] (f x `mzless` mzint 10)),
        Assert $ fromJust $ mznot (mzforall [x'] (f x `mzless` mzint 9)),
        CheckSat False,
        GetModel ]
    where
        ff          = Fun [] "f" [int] int
        f           = maybe1 $ (\x -> FunApp ff [x])

sample_proof = ProofObligation
        ( mk_context [FunDecl [] "f" [int] int] )
        [fromJust $ mzforall [x'] (f x `mzless` mzint 10)]
        False
        (fromJust $ mzforall [x'] (f x `mzless` mzint 12))
    where
        f           = maybe1 $ fun1 ff

sample_quant2 = [
        Decl (FunDecl [] "f" [int] int),
        Assert $ fromJust (mzforall [x'] (f x `mzless` mzint 10)),
        Assert $ fromJust (mzforall [x'] (f x `mzless` mzint 11)),
        CheckSat False]
    where
        f           = maybe1 $ fun1 $ Fun [] "f" [int] int

sample_quant3 = [
        Decl (FunDecl [] "f" [int] int),
        Assert $ fromJust (mzforall [x'] (f x `mzless` mzint 10)),
        Assert $ fromJust $ mznot (mzforall [x'] (f x `mzless` mzint 11)),
        CheckSat False]
    where
        f           = maybe1 $ fun1 $ Fun [] "f" [int] int
        

sample_calc = (Calc  
        ( mk_context [ FunDecl [] "f" [BOOL] BOOL,
              FunDef [] "follows" [x',y'] 
                BOOL (fromJust (y `mzimplies` x)),
              ConstDecl "x" BOOL,
              ConstDecl "y" BOOL ] )
        (  fromJust  ( (x `mzimplies` y) `mzimplies` (f x `mzimplies` f y) )  )
                   ( fromJust (f x `mzimplies` f y) )
        [ (Equal,    fromJust (f x `mzeq` (f x `mzand` f y)),  [], li)
        , (Equal,    fromJust ( f x `mzeq` f (x `mzand` y) ),  [hyp], li)
        , (Follows,  fromJust ( x `mzeq` (x `mzand` y) ), [], li)
        , (Equal,    fromJust ( x `mzimplies` y ),        [], li) 
        ]
        li )
    where
        hyp         = fromJust $ mzforall 
            [x',y'] 
            ( (f (x `mzand` y) `mzeq` (f x `mzand` f y)) )
        (x,x')      = var "x" BOOL
        (y,y')      = var "y" BOOL
        f           = maybe1 $ fun1 $ Fun [] "f" [BOOL] BOOL
        li          = (-1,-1)

indent xs = unlines (map (">  " ++) (lines xs))

type Result = (Either String Satisfiability, Either String Satisfiability, Validity, [(Validity, Int)])
   
main = do
        s1 <- verify sample_quant
        s2 <- verify sample_quant2
        s3 <- discharge sample_proof
        s4 <- check empty_theory sample_calc
        return (s1,s2,s3,s4)
