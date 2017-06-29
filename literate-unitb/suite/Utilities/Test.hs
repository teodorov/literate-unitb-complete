{-# LANGUAGE StandaloneDeriving #-}
module Utilities.Test (test_case) where

    -- Modules
import Logic.Operator
import Logic.OldOperator

import Logic.Theories.Arithmetic
import Logic.Theories.Notation
import Logic.Theories.SetTheory
import Logic.Theories.FunctionTheory

import Utilities.Graph as G
        ( as_map )

    -- Libraries
import Control.Lens

import           Data.Function
import qualified Data.Graph.Array as Graph
import           Data.List
import qualified Data.HashMap.Lazy as M
import qualified Data.Relation as Rel

import Test.UnitTest hiding (Node)

import Utilities.Error
import Utilities.Syntactic

import System.IO.Unsafe

result2 :: [((Operator,Operator),Assoc,Assoc)]
result2 = sortBy (compare `on` fst3) $ zip3 (map xbin_to_bin xs) ys zs
    where
        (xs,ys,zs) = unzip3
            [ ((And,Or),NoAssoc,LeftAssoc)
            , ((DomRest,DomRest),RightAssoc,LeftAssoc)
            , ((DomRest,DomSubt),RightAssoc,LeftAssoc)
            , ((DomRest,Membership),NoAssoc,LeftAssoc)
            , ((DomRest,SetDiff),NoAssoc,RightAssoc)
            , ((DomRest,Union),NoAssoc,RightAssoc)
            , ((DomSubt,DomRest),RightAssoc,LeftAssoc)
            , ((DomSubt,DomSubt),RightAssoc,LeftAssoc)
            , ((DomSubt,Membership),NoAssoc,LeftAssoc)
            , ((DomSubt,SetDiff),NoAssoc,RightAssoc)
            , ((DomSubt,Union),NoAssoc,RightAssoc)
            , ((Membership,DomRest),NoAssoc,RightAssoc)
            , ((Membership,DomSubt),NoAssoc,RightAssoc)
            , ((Membership,Mult),NoAssoc,RightAssoc)
            , ((Membership,Overload),NoAssoc,RightAssoc)
            , ((Membership,Plus),NoAssoc,RightAssoc)
            , ((Membership,Power),NoAssoc,RightAssoc)
            , ((Mult,Membership),NoAssoc,LeftAssoc)
            , ((Mult,SetDiff),NoAssoc,LeftAssoc)
            , ((Mult,Union),NoAssoc,LeftAssoc)
            , ((Or,And),NoAssoc,RightAssoc)
            , ((Overload,Membership),NoAssoc,LeftAssoc)
            , ((Overload,SetDiff),NoAssoc,LeftAssoc)
            , ((Overload,Union),NoAssoc,LeftAssoc)
            , ((Plus,Membership),NoAssoc,LeftAssoc)
            , ((Plus,SetDiff),NoAssoc,LeftAssoc)
            , ((Plus,Union),NoAssoc,LeftAssoc)
            , ((Power,Membership),NoAssoc,LeftAssoc)
            , ((Power,SetDiff),NoAssoc,LeftAssoc)
            , ((Power,Union),NoAssoc,LeftAssoc)
            , ((SetDiff,DomRest),NoAssoc,LeftAssoc)
            , ((SetDiff,DomSubt),NoAssoc,LeftAssoc)
            , ((SetDiff,Mult),NoAssoc,RightAssoc)
            , ((SetDiff,Overload),NoAssoc,RightAssoc)
            , ((SetDiff,Plus),NoAssoc,RightAssoc)
            , ((SetDiff,Power),NoAssoc,RightAssoc)
            , ((SetDiff,Union),NoAssoc,RightAssoc)
            , ((Union,DomRest),NoAssoc,LeftAssoc)
            , ((Union,DomSubt),NoAssoc,LeftAssoc)
            , ((Union,Mult),NoAssoc,RightAssoc)
            , ((Union,Overload),NoAssoc,RightAssoc)
            , ((Union,Plus),NoAssoc,RightAssoc)
            , ((Union,Power),NoAssoc,RightAssoc)
            , ((Union,SetDiff),NoAssoc,LeftAssoc)
            ]

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
            
case1 :: IO [((Operator,Operator),Assoc,Assoc)]
case1 = return $ result
    where
        result = sortBy (compare `on` fst3) $ zip3 xs (map (g0 M.!) xs) (map (g1 M.!) xs)
        g0   = as_map $ notations^.struct
        g1   = assoc0
        rs   = M.keys assoc0
        eq x = g0 `lu` x /= g1 `lu` x
        xs   = filter eq rs
        lu x y = case M.lookup y x of
            Just z  -> z
            Nothing -> unsafePerformIO $ do
                print y
                return undefined

case2 :: IO [((Operator,Operator),Assoc,Assoc)]
case2 = do
        xs <- case1
        return $ filter h xs
    where
        f ((x,y),_,_) = null $ [x,y] `intersect` map Right [total_fun,mk_fun_op]
        g ((x,y),_,_) = not $ (x,y) `elem` combos
        h x = f x && g x
        combos = concat [ [(i,j),(j,i)] | i <- rel, j <- set ] 
        rel = map Right [ geq, leq, less, greater ]
        set = map Right [ set_union, domrest, domsubt, set_diff, membership, overload ]

case5   :: IO (Either [Error] ((), [Error]))
result5 :: Either [Error] ((), [Error])

(case5,result5) = (test,result)
    where
        test = runErrorT $ do
            soft_error [e0]
            make_soft () $ do
                soft_error [e2]
                hard_error [e0]
            li <- hard_error [e1]
            soft_error [Error "error d" li]
        result = Left [e0,e2,e0,e1]
        li = LI "file.ext" 1 2
        e0 = Error "error a" li
        e1 = Error "error b" li
        e2 = Error "error c" li

test_case :: TestCase
test_case = test_cases "Graphs and operator grammars" $
    [ aCase "case 2 - new ambiguities" case2 result2
    , aCase "case 5 - error monad" case5 result5
    , QuickCheckProps "case 11 - Relations, quickcheck" Rel.run_spec
    , QuickCheckProps "case 12 - New graphs, quickcheck" Graph.run_tests ]
