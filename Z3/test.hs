module Z3.Test where

import Tests.UnitTest

import Z3.Z3 as Z
import Z3.Def
import Z3.Const

import Z3.Calculation

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

a        = Word (Var "a" INT)
(x,x')   = var "x" INT
ff       = Fun "f" [INT, BOOL] INT

sample_ast = [
        Decl (ConstDecl "a" INT),
        Decl (FunDecl "f" [INT, BOOL] INT),
        Assert (a `zgreater` zint 10),
        Assert (f a ztrue `zless` zint 10),
        CheckSat False ]
    where
        f       = fun2 ff

sample_quant = [
        Decl (FunDecl "f" [INT] INT),
        Assert (forall [x'] (f x `zless` zint 10)),
        Assert $ znot (forall [x'] (f x `zless` zint 9)),
        CheckSat False,
        GetModel ]
    where
        ff          = Fun "f" [INT] INT
        f x         = FunApp ff [x]
        forall      = Binder Forall 

sample_proof = ProofObligation
        ( Z.context [FunDecl "f" [INT] INT] )
        [forall [x'] (f x `zless` zint 10)]
        False
        (forall [x'] (f x `zless` zint 12))
    where
        f           = fun1 ff
        forall      = Binder Forall 

sample_quant2 = [
        Decl (FunDecl "f" [INT] INT),
        Assert (forall [x'] (f x `zless` zint 10)),
        Assert (forall [x'] (f x `zless` zint 11)),
        CheckSat False]
    where
        f           = fun1 $ Fun "f" [INT] INT
        forall      = Binder Forall 

sample_quant3 = [
        Decl (FunDecl "f" [INT] INT),
        Assert (forall [x'] (f x `zless` zint 10)),
        Assert $ znot (forall [x'] (f x `zless` zint 11)),
        CheckSat False]
    where
        f           = fun1 $ Fun "f" [INT] INT
        forall      = Binder Forall 
        

sample_calc = (Calc  
        ( Z.context [ FunDecl "f" [BOOL] BOOL,
              FunDef "follows" [x',y'] 
                BOOL (y `zimplies` x),
              ConstDecl "x" BOOL,
              ConstDecl "y" BOOL ] )
        ( (x `zimplies` y) `zimplies` (f x `zimplies` f y) )
                     (f x `zimplies` f y)
        [ (Equal,    (f x `zeq` (f x `zand` f y)),  [], li),
          (Equal,    ( f x `zeq` f (x `zand` y) ),  [hyp], li),
          (Follows,  ( x `zeq` (x `zand` y) ), [], li),
          (Equal,    ( x `zimplies` y ),        [], li) ]
        li )
    where
        forall      = Binder Forall 
        hyp         = forall 
            [x',y'] 
            ( (f (x `zand` y) `zeq` (f x `zand` f y)) )
        (x,x')      = var "x" BOOL
        (y,y')      = var "y" BOOL
        f           = fun1 $ Fun "f" [BOOL] BOOL
        li          = (-1,-1)

indent xs = unlines (map (">  " ++) (lines xs))

type Result = (Either String Satisfiability, Either String Satisfiability, Validity, [(Validity, Int)])
   
cases = test_cases [case0, case1, case2, case3, case4]

test_case = ("Z3 test", cases, True)

case0 = Case "sample_quant" (verify sample_quant) $ Right Sat
case1 = Case "sample_quant2" (verify sample_quant2) $ Right Sat
case2 = Case "sample_quant3" (verify sample_quant3) $ Right Unsat
case3 = Case "sample proof" (discharge sample_proof) Valid

case4 = Case "check sample calc" (check sample_calc) []

main = do
        s1 <- verify sample_quant
        s2 <- verify sample_quant2
        s3 <- discharge sample_proof
        s4 <- check sample_calc
        return (s1,s2,s3,s4)