{-# LANGUAGE OverloadedStrings #-}
module Language.UnitB.Test where 

    -- Modules
import Language.UnitB.Parser.Tests.Suite (lookupSequent)

import           Logic.Expr
import qualified Logic.Expr.Const as Exp
import           Logic.Expr.Parser
import           Logic.Expr.Existential
import           Logic.Proof.POGenerator hiding (variables)
import qualified Logic.Proof.POGenerator as POG
import           Logic.UnitTest

import Logic.Theories.FunctionTheory

import Language.UnitB.PO (prop_saf')
import Language.UnitB.QuasiQuote
import Language.UnitB

import Z3.Z3

    -- Libraries
import Control.Monad
import Control.Lens hiding (indices)
import Control.Lens.Misc
import Control.Precondition

import           Data.List ( sort )
import qualified Data.List.NonEmpty as NE
import           Data.Map  as M hiding (map)
import           Data.Text (Text)

import Test.UnitTest

import Utilities.Syntactic

test_case :: TestCase
test_case = test

test :: TestCase
test = test_cases 
        "Unit-B" 
        [  poCase "0: 'x eventually increases' verifies" (check_mch example0) (result_example0)
        ,  poCase "1: train, model 0, verification" (check_mch train_m0) (result_train_m0)
        ,  textCase "2: train, m0 transient / enablement PO" (get_tr_en_po train_m0) result_train_m0_tr_en_po
        ,  textCase "3: train, m0 transient / falsification PO" (get_tr_neg_po train_m0) result_train_m0_tr_neg_po
        ,  aCase "4: Feasibility and partitioning" case3 result3
        ,  aCase "5: Debugging the partitioning" case4 result4
        ,  aCase "6: unless with except and split event" case5 result5
        ]

example0 :: Either [Error] RawMachine
example0 = do
        let (x,x',x_decl) = prog_var "x" int
            (y,_,y_decl) = prog_var "y" int
            li = LI "" 0 0
        inv0   <- with_li li (x `mzeq` (mzint (2 :: Int) `mztimes` y))
        init0  <- with_li li (x `mzeq` mzint (0 :: Int))
        init1  <- with_li li (y `mzeq` mzint (0 :: Int))
        tr     <- with_li li (x `mzeq` mzint (0 :: Int))
        co     <- with_li li (x `mzle` x')
        csched <- with_li li (x `mzeq` y)
        s0     <- with_li li (liftM (Assign x_decl) (x `mzplus` mzint (2 :: Int)))
        s1     <- with_li li (liftM (Assign y_decl) (y `mzplus` mzint (1 :: Int)))
        let tr0 = Tr empty tr (NE.fromList ["evt"]) empty_hint
            co0 = Co [] co
            ps = empty_property_set {
                _transient = 
                    fromList [
                        ("TR0", tr0)],
                _constraint =
                    fromList [
                        ("CO0", co0)],
                _inv = fromList [("J0", inv0)] }
            evt = create $ do
                    coarse_sched .= singleton "sch0" csched
                    actions .= fromList [
                        ("S0", s0),
                        ("S1", s1) ]
            vs = fromList $ map as_pair [x_decl,y_decl]
            m  = newMachine (fromString'' "m0") $ do
                variables .= vs
                event_table .= new_event_set vs (singleton "evt" evt)
                inits .= fromList 
                    [ ("init0", init0)
                    , ("init1", init1) ]
                props .= ps
        return m 

select :: ExprP -> ExprP -> ExprP
select = typ_fun2 (mk_fun' [] "select" [array gA gB, gA] gB)

train_m0 :: Either [Error] RawMachine
train_m0 = do
        let (st,_,st_decl) = prog_var "st" (array int bool)
            (t,t_decl) = Exp.var "t" int
        let li = LI "" 0 0
        inv0 <- with_li li (mzforall [t_decl] mztrue $
                   mzall [(zstore st t mzfalse `mzeq` zstore st t mzfalse)])
        c0   <- with_li li (st `select` t)
        a0   <- with_li li (liftM (Assign st_decl) $ zstore st t mzfalse)
        let inv = fromList [("J0",inv0)]
            enter = ("enter", empty_event)
            leave = ("leave", create $ do
                        indices .= symbol_table [t_decl]
                        coarse_sched .= singleton "C0" c0
                        actions .= fromList [("A0", a0)]
                    )
        tr <- with_li li (st `select` t)
        let props' = fromList [("TR0", Tr (symbol_table [t_decl]) tr (NE.fromList ["leave"]) hint)] 
            hint  = getExpr <$> TrHint (singleton (fromString'' "t") (int,c [expr| t = t' |])) Nothing
            c     = ctx $ do
                    [var| t, t' : \Int |]
            ps    = empty_property_set { _transient = props', _inv = inv }
            vs    = fromList $ map as_pair [st_decl]
            m     = newMachine (fromString'' "train_m0") $ do
                        props     .= ps
                        variables .= vs
                        event_table .= new_event_set vs (fromList [enter, leave])
        return m

result_example0 :: Output
result_example0 = readFileLn [path|expected/Language/UnitB/result_example0.txt|]

result_train_m0 :: Output
result_train_m0 = readFileLn [path|expected/Language/UnitB/result_train_m0.txt|]

result_example0_tr_en_po :: Output
result_example0_tr_en_po = readFileLn [path|expected/Language/UnitB/result_example0_tr_en_po.txt|]

result_train_m0_tr_en_po :: Output
result_train_m0_tr_en_po = readFileLn [path|expected/Language/UnitB/result_train_m0_tr_en_po.txt|]

result_train_m0_tr_neg_po :: Output
result_train_m0_tr_neg_po = readFileLn [path|expected/Language/UnitB/result_train_m0_tr_neg_po.txt|]

check_mch :: Either [Error] RawMachine -> IO (Text, Map Label Sequent)
check_mch em = do
    case em of
        Right m -> do
            let r = proof_obligation m
            (xs,_,_) <- str_verify_machine m
            return (xs,r)
        Left x -> return (show_err x, empty)

get_cmd_tr_po :: Monad m 
              => Either [Error] RawMachine 
              -> m (Either [Error] Text)
get_cmd_tr_po em = return (do
        m <- em
        let lbl = composite_label [as_label $ _name m, "TR/TR0/t@param"]
            pos = proof_obligation m
            po  = lookupSequent lbl pos
            cmd = either id z3_code po
        return cmd)
    
get_tr_en_po :: Either [Error] RawMachine -> IO Text
get_tr_en_po em = either (return . show_err) return $ do
        m   <- em
        let lbl = composite_label [as_label $ _name m, "TR0/TR/leave/EN"]
            pos = proof_obligation m
            po  = either id prettyText $ lookupSequent lbl pos
        return $ po

get_tr_neg_po :: Either [Error] RawMachine -> IO Text
get_tr_neg_po em = either (return . show_err) return $ do
        m   <- em
        let lbl = composite_label [as_label $ _name m, "TR0/TR/leave/NEG"]
            pos = proof_obligation m
            po  = either id prettyText $ lookupSequent lbl pos
        return po

case3 :: IO [([Var], [Expr])]
result3 :: [([Var], [Expr])]
case4 :: IO ([(Int, Int)], [(Var, Int)], [(Expr, Int)])
result4 :: ([(Int, Int)], [(Var, Int)], [(Expr, Int)])
(case3, result3, case4, result4) = fromRight' $ do
            e0 <- a
            e1 <- d `mzplus` b
            e2 <- b `mzplus` c
            e3 <- c `mzplus` d
            let arg0 = [a_decl,b_decl,c_decl,d_decl] 
                arg1 = [e0,e1,e2,e3]
            return 
                ( return $ map f $ partition_expr arg0 arg1 & traverse._2 %~ NE.toList
                , [([a_decl],[e0]),([b_decl,c_decl,d_decl],[e2,e3,e1])]
                , return $ get_partition arg0 arg1
                , ( [ (0,0), (1,1)
                    , (2,1), (3,1)
                    , (4,0), (5,1)
                    , (6,1), (7,1)]
                , [ (a_decl,0), (b_decl,1)
                  , (c_decl,2), (d_decl,3)]
                , [ (e0,4), (e2,6), (e3,7)
                    , (e1,5)]) )
    where
        (a,a_decl) = Exp.var "a" int
        (b,b_decl) = Exp.var "b" int
        (c,c_decl) = Exp.var "c" int
        (d,d_decl) = Exp.var "d" int
        f (xs,ys) = (sort xs, sort ys)

result5 :: Map Label Sequent
result5 = eval_generator $ with (do
            POG.variables $ symbol_table
                [ z3Var "p" bool
                , z3Var "q" bool
                , z3Var "p'" bool
                , z3Var "q'" bool ]
            named_hyps $ fromList 
                [ ("SKIP:p", c [expr| p' = p |] ) 
                , ("SKIP:q", c [expr| q' = q |])]
            ) $ do
        emit_goal ["ce0a/SAF/lbl"] ztrue
        emit_goal ["ce0b/SAF/lbl"] ztrue
        let p = c [expr| p \land \neg q \1\implies p' \lor q' |]
        emit_goal' ["ce1a/SAF/lbl"] p
        emit_goal' ["ce1b/SAF/lbl"] p
    where
        c p = c' $ p . (is_step .~ True)
        c' = ctx (do
            primable [var| p, q : \Bool |])

case5 :: IO (Map Label Sequent)
case5 = return $ eval_generator 
        $ prop_saf' m (Just "ae0") ("lbl", getExpr <$> c [safe| p UNLESS q |])
    where
        c = ctx $ do
                [var| p, q : \Bool |]
        m = newMachine (fromString'' "m0") $ do
            variables .= symbol_table [z3Var "p" bool,z3Var "q" bool]
            event_table .= eventMap (do
                split_event "ae0" (return ()) 
                    [ ("ce0a",return ()) 
                    , ("ce0b",return ()) ]
                split_event "ae1" (return ())
                    [ ("ce1a",return ()) 
                    , ("ce1b",return ()) ] )
