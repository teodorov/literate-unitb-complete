{-# LANGUAGE FlexibleContexts #-}
module UnitB.ExpressionStore 
    ( ExprStore, insert, empty_store, get_string )
where

    -- Modules
import Z3.Def
import Z3.Const

import UnitB.Label
import UnitB.Theory ()

    -- Libraries
import Control.Monad.State.Class

import Data.Map as M ( insertWith, Map, empty, lookup ) --, (!) )
--import Data.List ( splitAt )

data UntypedExpr = UE Expr

instance Show UntypedExpr where
    show (UE e) = show e

instance Eq UntypedExpr where
    UE (Word (Var n0 _)) == UE (Word (Var n1 _)) = n0 == n1
    UE (Const _ n0 _) == UE (Const _ n1 _)       = n0 == n1
    UE (FunApp (Fun _ f0 _ _) arg0) 
     == UE (FunApp (Fun _ f1 _ _) arg1)          = 
            f0 == f1
         && map UE arg0 == map UE arg1
    UE (Binder q0 v0 r0 t0) 
     == UE (Binder q1 v1 r1 t1)                  =
            q0 == q1
         && map name v0 == map name v1
         && UE r0 == UE r1
         && UE t0 == UE t1
    _ == _ = False

instance Ord UntypedExpr where
    compare (UE (Word (Var n0 _))) (UE (Word (Var n1 _))) 
        = compare n0 n1
    compare (UE (Const _ n0 _)) (UE (Const _ n1 _))       
        = compare n0 n1
    compare (UE (FunApp (Fun _ f0 _ _) arg0)) 
            (UE (FunApp (Fun _ f1 _ _) arg1))
        = compare 
             (f0,map UE arg0)
             (f1,map UE arg1)
    compare (UE (Binder q0 v0 r0 t0))
            (UE (Binder q1 v1 r1 t1))
        = compare 
            (q0,map name v0,UE r0,UE t0)
            (q1,map name v1,UE r1,UE t1)
    compare (UE x) (UE y) = compare x y

data ExprStore = ExprStore { getMap :: Map UntypedExpr [String] }
    deriving Eq

empty_store = ExprStore empty

insert :: ( Monad m
          , MonadState ExprStore m )
       => Expr -> String -> m ()
insert e str = modify $ ExprStore . insertWith (++) (UE e) [str] . getMap

get_string :: ( Monad m
              , MonadState ExprStore m )
           => Expr -> m String
get_string e = gets $ maybe (to_latex e) head . (M.lookup $ UE e) . getMap

--(!) x y = maybe (error $ "ExprStore: expression not found: " ++ show y) id $ M.lookup y x

generated_by = "" --  % this is generated by Literate Unit-B"

to_latex (FunApp (Fun _ "=" _ _) [Word (Var x _),Word (Var y _)])
        = primed x ++ " = " ++ primed y ++ generated_by
    where
        prime = "@prime"
        primed x
                | suffix == prime = prefix ++ "'"
                | otherwise       = x
            where
                (prefix, suffix) = splitAt (length x - length prime) x
to_latex e 
    | e == zfalse = "\false " ++ generated_by
    | otherwise   = error $ "ExprStore.to_latex: cannot convert expression to LaTeX: " ++ show e