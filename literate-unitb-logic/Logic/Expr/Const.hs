{-# LANGUAGE ScopedTypeVariables,ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Logic.Expr.Const where

    -- Modules   
import Logic.Expr.Classes 
import Logic.Expr.Expr
import Logic.Expr.Genericity
import Logic.Expr.PrettyPrint
import Logic.Expr.Prism
import Logic.Expr.Type

    -- Libraries
import Control.Applicative 
import Control.Lens hiding (rewrite)
import Control.Lens.Misc
import Control.Monad 
import Control.Precondition

import           Data.Foldable as F
import           Data.List as L
import           Utilities.MapSyntax
import qualified Data.HashMap.Lazy as M
import qualified Data.Set as S
import           Data.Text as T (unlines,unpack)

import Text.Printf.TH

import Utilities.Syntactic

infixr 1 .==.
infix 2 .=>
infixr 3 \/
infixr 3 /\
infix 4 .=.
infix 4 .<=
infix 4 .<
infixr 5 .+
infixr 6 .*
infixl 7 .^

type OneExpr n t q = AbsExpr n t q -> AbsExpr n t q

type TwoExpr n t q = AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q

type TwoGenExpr n t a q = GenExpr n t a q -> GenExpr n t a q -> GenExpr n t a q

fun1 :: AbsFun n a
     -> GenExpr n t a q -> GenExpr n t a q
fun1 f x           = funApp f [x]
fun2 :: AbsFun n a -> GenExpr n t a q
     -> GenExpr n t a q -> GenExpr n t a q
fun2 f x y         = funApp f [x,y]

fun2' :: AbsFun n a -> GenExpr n t a q
      -> GenExpr n t a q -> GenExpr n t a q
fun2' f x y         = FunApp f [x,y]

no_errors2 :: ( TypeSystem t 
              , IsQuantifier q )
           => (TwoExprP n t q)
           -> (TwoExpr n t q)
no_errors2 f x y = either (error . unpack . T.unlines) id $ f (Right x) (Right y)

toErrors :: LineInfo -> ExprP -> Either [Error] Expr
toErrors li m = case m of
        Right x -> Right x
        Left xs -> Left $ map (`Error` li) xs

not_fun :: (TypeSystem2 t,IsName n) => AbsFun n t
not_fun = mk_fun [] [smt|not|] [bool] bool

znot :: (IsName n,TypeSystem2 a) 
     => GenExpr n t a q
     -> GenExpr n t a q
znot e = case e of 
            FunApp f [x]
                | f == not_fun -> x
                | otherwise    -> fun1 not_fun e
            e' -> fun1 not_fun e'
    -- znot         = fun1 mznot
zimplies :: (TypeSystem2 t, IsQuantifier q,IsName n,TypeAnnotationPair t a,TypeSystem2 a) 
         => TwoGenExpr n t a q
zimplies x y = runIdentity $ mzimplies' (liftA2 $ fun2 implies_fun) (pure x) (pure y)
zand :: (TypeSystem2 t, IsQuantifier q,IsName n) => TwoExpr n t q
zand x y     = zall [x, y]
zor :: (TypeSystem2 t, IsQuantifier q,IsName n) => TwoExpr n t q
zor x y      = zsome [x, y]

zeq_fun :: (IsName n,TypeSystem t) => t -> AbsFun n t
zeq_fun t    = mk_fun [] [smt|=|] [t, t] bool

zeq_symb :: (IsQuantifier q,IsName n) => TwoExpr n Type q
zeq_symb     = no_errors2 mzeq_symb
mzeq_symb :: IsName n => TwoExprP n Type q
mzeq_symb    = typ_fun2 $ mk_fun [gA] [smt|eq|] [gA, gA] bool

zeq :: (IsQuantifier q,IsName n,TypeSystem t)
    => AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q
zeq x y = provided (type_of x == type_of y) $ funApp (zeq_fun $ type_of x) [x,y]
mzeq :: IsName n => TwoExprP n Type q
mzeq         = typ_fun2 (zeq_fun gA)

(.=.) :: IsName n => TwoExprP n Type q
(.=.) = mzeq
(.==.) :: IsName n => TwoExprP n Type q
(.==.) = mzeq

zfollows :: (IsName n,TypeSystem2 t) => AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q
zfollows     = fun2 $ mk_fun [] [smt|follows|] [bool,bool] bool

and_fun :: (IsName n,TypeSystem t) => Int -> AbsFun n t
and_fun n = mk_fun [] [smt|and|] (replicate n bool) bool

zall :: ( TypeSystem t, TypeSystem a
        , TypeAnnotationPair t a
        , IsQuantifier q, Foldable list,IsName n)
     => list (GenExpr n t a q) -> GenExpr n t a q
zall xs'      = 
        case xs of
            []  -> ztrue
            [x] -> x
            xs  
                | zfalse `elem` xs -> zfalse
                | otherwise -> applyAssoc and_fun xs
    where
        xs = concatMap f $ F.toList xs'
        f (FunApp fun@(Fun [] n _ _ _ _) xs)
            | render n == "and" 
                && not (isLifted fun) = concatMap f xs
        f x
            | x == ztrue = []
            | otherwise   = [x]

or_fun :: (IsName n,TypeSystem t) => Int -> AbsFun n t
or_fun n = mk_fun [] [smt|or|] (replicate n bool) bool

zsome :: (TypeSystem2 t, IsQuantifier q, Traversable list,IsName n) 
      => list (AbsExpr n t q) -> AbsExpr n t q
zsome xs'      = 
        case concatMap f xs of
            []  -> zfalse
            [x] -> x
            xs
                | ztrue `elem` xs -> ztrue
                | otherwise        -> applyAssoc or_fun xs
    where
        xs = toList xs'
        f (FunApp fun@(Fun [] n _ _ _ _) xs) 
            | n == [smt|or|]
                && not (isLifted fun) = concatMap f xs
        f x
            | x == zfalse = []
            | otherwise   = [x]
zforall :: (TypeSystem2 t, IsQuantifier q,IsName n,TypeAnnotationPair t a, TypeSystem2 a)
        => [AbsVar n t] 
        -> TwoGenExpr n t a q
zforall [] x y  = zimplies x y
zforall vs x w@(Binder q us y z _) 
    | q == qForall = if x == ztrue
            then zforall (vs ++ us) y z
            else binder qForall vs x w bool
zforall vs x w   
    |    x `elem` [ztrue, zfalse]
      && w `elem` [ztrue, zfalse] = zimplies x w
    | otherwise                   = binder qForall vs x w bool

zexists :: (TypeSystem2 t, IsQuantifier q,IsName n)
        => [AbsVar n t] 
        -> AbsExpr n t q
        -> AbsExpr n t q
        -> AbsExpr n t q
zexists [] x y = zand x y
zexists vs x w@(Binder q us y z _) 
    | q == qExists = if x == ztrue 
                        then zexists (vs ++ us) y z
                        else Binder qExists vs x w bool
zexists vs x w   
    |    x `elem` [ztrue, zfalse]
      && w `elem` [ztrue, zfalse] = zand x w
    | otherwise                   = Binder qExists vs x w bool

zquantifier :: HOQuantifier -> [Var] -> ExprP -> ExprP -> ExprP
zquantifier q vs r t = do
    r' <- zcast bool r
    t' <- zcast (termType q) t
    let tuple = ztuple_type (map var_type vs)
        rt    = exprType q tuple (type_of t')
    return $ binder q vs r' t' rt

ite_fun :: IsName n => AbsFun n Type
ite_fun = mk_fun [] [smt|ite|] [bool,gA,gA] gA

zite :: IsName n => ThreeExprP n Type q
zite       = typ_fun3 ite_fun

zjust :: IsName n => OneExprP n Type q
zjust      = typ_fun1 (mk_fun [] [smt|Just|] [gA] (maybe_type gA))

znothing :: IsName n => ExprPG n Type q
znothing   = Right $ Cast CodeGen (funApp (mk_fun [] [smt|Nothing|] [] $ maybe_type gA) []) (maybe_type gA)

mznot :: (TypeSystem2 t,IsName n) => OneExprP n t q
mznot me       = do
        e <- me
        case e of
            FunApp f [x] 
                | f == not_fun -> typ_fun1 not_fun (Right x)
            e -> typ_fun1 not_fun (Right e)
mzimplies :: (TypeSystem2 t,IsName n,IsQuantifier q)
          => ExprPG n t q -> ExprPG n t q -> ExprPG n t q
mzimplies = mzimplies' (typ_fun2 implies_fun)
mzimplies' :: (Monad m,TypeAnnotationPair t a,TypeSystem2 t,IsName n,IsQuantifier q,TypeSystem2 a)
           => (m (GenExpr n t a q) -> m (GenExpr n t a q) -> m (GenExpr n t a q))
           -> m (GenExpr n t a q) -> m (GenExpr n t a q) -> m (GenExpr n t a q)
mzimplies' mkImplies mx my = do
        x <- mx
        y <- my
        if      x == ztrue  then pure $ y
        else if y == ztrue  then pure $ ztrue
        else if x == zfalse then pure $ ztrue
        else if y == zfalse then pure $ znot x
        else mkImplies (pure x) (pure y)

implies_fun :: (IsName n,TypeSystem t) => AbsFun n t
implies_fun = mk_fun [] [smt|=>|] [bool,bool] bool

(.=>) :: (TypeSystem2 t,IsName n) => TwoExprP n t q
(.=>) = mzimplies

mzand :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mzand x y     = mzall [x,y]
mzor :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mzor x y      = mzsome [x,y]

(\/) :: (TypeSystem2 t,IsName n) => TwoExprP n t q
(\/) = mzor

(/\) :: (TypeSystem2 t,IsName n) => TwoExprP n t q
(/\) = mzand

mzfollows :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mzfollows x y = mzimplies y x
mztrue :: (TypeSystem2 t,IsQuantifier q,IsName n)
       => ExprPG n t q
mztrue        = Right ztrue
mzfalse :: (TypeSystem2 t,IsQuantifier q,IsName n)
        => ExprPG n t q
mzfalse       = Right zfalse

mzall :: (IsQuantifier q, TypeSystem2 t, Traversable list,IsName n) 
      => list (ExprPG n t q) -> ExprPG n t q
mzall xs = case toList xs of
    []  -> mztrue
    [x] -> x
    xs  -> do
        xs <- forM xs $ zcast bool 
        return $ zall xs

mzsome :: (IsQuantifier q, TypeSystem2 t, Traversable list,IsName n) 
       => list (ExprPG n t q) -> ExprPG n t q
mzsome xs = case toList xs of
    []  -> mzfalse
    [x] -> x
    xs  -> do
        xs <- forM xs $ zcast bool
        return $ zsome xs

mzforall :: (TypeSystem2 t,IsName n) 
         => [AbsVar n t] 
         -> TwoExprP n t q
mzforall xs mx my = do
        x <- zcast bool mx
        y <- zcast bool my
        return $ zforall xs x y

mzexists :: (TypeSystem2 t,IsName n)
         => [AbsVar n t] 
         -> TwoExprP n t q
mzexists xs mx my = do
        x <- zcast bool mx
        y <- zcast bool my
        return $ zexists xs x y

zless :: (IsName n,TypeSystem2 t) => AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q
zless        = fun2 $ mk_fun [] [smt|<|] [int,int] bool

zgreater :: (IsName n,TypeSystem2 t) => AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q
zgreater     = fun2 $ mk_fun [] [smt|>|] [int,int] bool

le_fun :: (IsName n,TypeSystem t) => AbsFun n t
le_fun = mk_fun [] [smt|<=|] [int,int] bool

zle :: (IsName n,TypeSystem2 t) => AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q
zle          = fun2 le_fun

zge :: (IsName n,TypeSystem2 t) => AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q
zge          = fun2 $ mk_fun [] [smt|>=|] [int,int] bool

zplus :: (IsName n,TypeSystem2 t )=> AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q
zplus        = fun2 $ mk_fun [] [smt|+|] [int,int] int

zminus :: (IsName n,TypeSystem2 t )=> AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q
zminus       = fun2 $ mk_fun [] [smt|-|] [int,int] int

zopp :: (IsName n,TypeSystem a )=> GenExpr n t a q -> GenExpr n t a q
zopp         = fun1 $ mk_fun [] [smt|-|] [int] int

ztimes :: (IsName n,TypeSystem2 t )=> AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q
ztimes       = fun2 $ mk_fun [] [smt|*|] [int,int] int

zpow :: (IsName n,TypeSystem2 t )=> AbsExpr n t q -> AbsExpr n t q -> AbsExpr n t q
zpow         = fun2 $ mk_fun [] [smt|^|] [int,int] int

zselect :: IsName n => TwoExprP n Type q
zselect      = typ_fun2 (mk_fun [] [smt|select|] [array gA gB, gA] gB)

zint :: (TypeSystem t) => Int -> GenExpr n t a q
zint n       = Lit (IntVal n) int

zreal :: TypeSystem t => Double -> AbsExpr n t q
zreal n      = Lit (RealVal n) real

mzless :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mzless        = typ_fun2 less_fun
(.<) :: (TypeSystem2 t,IsName n) => TwoExprP n t q
(.<) = mzless

mzgreater :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mzgreater        = typ_fun2 $ mk_fun [] [smt|>|] [int,int] bool
mzle :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mzle          = typ_fun2 le_fun
(.<=) :: (TypeSystem2 t,IsName n) => TwoExprP n t q
(.<=) = mzle

mzge :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mzge          = typ_fun2 $ mk_fun [] [smt|>=|] [int,int] bool
mzplus :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mzplus       = typ_fun2 plus_fun
(.+) :: (TypeSystem2 t,IsName n) => TwoExprP n t q
(.+)       = mzplus

mzminus :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mzminus       = typ_fun2 minus_fun
mzopp :: (TypeSystem2 t,IsQuantifier q,IsName n) => ExprPG n t q -> ExprPG n t q
mzopp         = typ_fun1 $ mk_fun [] [smt|-|] [int] int
mztimes :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mztimes       = typ_fun2 times_fun
(.*) :: (TypeSystem2 t,IsName n) => TwoExprP n t q
(.*) = mztimes

mzpow :: (TypeSystem2 t,IsName n) => TwoExprP n t q
mzpow         = typ_fun2 pow_fun
(.^) :: (TypeSystem2 t,IsName n) => TwoExprP n t q
(.^) = mzpow

pow_fun,times_fun,plus_fun,minus_fun,less_fun,prefix_minus_fun
    :: (IsName n,TypeSystem t) => AbsFun n t
pow_fun          = mk_fun [] [smt|^|] [int,int] int
times_fun        = mk_fun [] [smt|*|] [int,int] int
plus_fun         = mk_fun [] [smt|+|] [int,int] int
minus_fun        = mk_fun [] [smt|-|] [int,int] int
less_fun         = mk_fun [] [smt|<|] [int,int] bool
prefix_minus_fun = mk_fun [] [smt|-|] [int] int

mzint :: (TypeSystem2 t) => Int -> ExprPG n t q 
mzint n       = Right $ zint n

mzreal :: TypeSystem2 t => Int -> ExprPG n t q
mzreal x       = Right $ zreal $ fromIntegral x

pair_fun :: Fun
pair_fun = mk_fun [] [smt|pair|] [gA,gB] (pair_type gA gB)

mzpair :: ExprP -> ExprP -> ExprP
mzpair = typ_fun2 pair_fun

var_of :: AbsExpr n t q -> AbsVar n t
var_of (Word v) = v
var_of _ = error "var_of: expecting a variable expression"

-- {-# DEPRECATED var, prog_var "use Logic.QuasiQuote" #-}
var :: (Pre,IsName n) => String -> t -> (Either a (AbsExpr n t q), AbsVar n t)
var n t      = (Right $ Word $ v, v)
    where
        v = Var (z3Name n) t

prog_var :: String -> Type -> (Either a Expr, Either a Expr, Var)
prog_var n t = (Right $ Word v, Right $ Word $ prime v, v)
    where
        v = Var (fromString'' n) t

apply_fun :: Fun
apply_fun = mk_fun [gA,gB] [smt|apply|] [fun_type gA gB, gA] gB

zapply :: ExprP -> ExprP -> ExprP
zapply  = typ_fun2 apply_fun

one_point_rule :: forall n t q. (IsQuantifier q, TypeSystem2 t,IsName n) 
               => AbsExpr n t q -> AbsExpr n t q
one_point_rule = foo . one_point_rule'
    where
        foo e = case rewrite foo e of
                    [fun| (= $lhs $rhs) |] 
                        | lhs == rhs -> ztrue
                        --  lhs == ztrue -> lhs
                        --  lhs == ztrue -> lhs
                    Binder q _ x y _
                        | q == qExists && x == ztrue && y == ztrue     -> ztrue
                        | q == qExists && (x == zfalse || y == zfalse) -> zfalse
                        | q == qForall && (x == zfalse || y == ztrue)  -> ztrue
                        | q == qForall && x == ztrue && y == zfalse    -> zfalse
                    FunApp fun xs
                        | (fun^.name) == [smt|and|] -> zall xs
                        | (fun^.name) == [smt|or|]  -> zsome xs
                        | (fun^.name) == [smt|not|] -> znot $ head xs
                        | (fun^.name) == [smt|=>|]  -> zimplies (xs !! 0) (xs !! 1)
                    e' -> e'

one_point_rule' :: forall n t q. (IsQuantifier q, TypeSystem2 t,IsName n) 
                => AbsExpr n t q -> AbsExpr n t q
one_point_rule' (Binder q vs r t _) 
        | q == qExists = e
    where
        e  = zsome [ f $ zexists (filter (`S.member` fv) vs \\ M.keys inst) ztrue 
                        $ zall $ map (substitute 
                                        $ M.mapKeys (view name) inst) ts
                   | (inst,ts,fv) <- insts ]
        
        insts :: [ ( M.HashMap (AbsVar n t) (AbsExpr n t q)
                   , [AbsExpr n t q]
                   , S.Set (AbsVar n t)) ]
        insts = [ (M.unions $ map subst ts,ts,S.unions $ map used_var ts) | ts <- ts' ]
        
        subst :: AbsExpr n t q -> M.HashMap (AbsVar n t) (AbsExpr n t q)
        subst (FunApp f xs)
                | (z3_name f) == [smt|=|] = M.fromList $ rs
            where
                rs = do (i,j) <- [(0,1),(1,0)]
                        k <- maybeToList 
                            $ (xs ! i) `lookup` zip (map Word vs) vs
                        guard $ S.null $ S.intersection (S.fromList vs) (used_var $ xs ! j)
                        return (k, xs ! j)
        subst _ = M.empty
        f x
            | length ts' == 1   = rewrite one_point_rule' x
            | otherwise         = one_point_rule' x
        ts  = conjuncts r ++ conjuncts t
        ts' = forM (map disjuncts ts) id
one_point_rule' e = rewrite one_point_rule' e

conjuncts :: (IsName n,TypeSystem t) => AbsExpr n t q -> [AbsExpr n t q]
conjuncts (FunApp f xs) 
    | z3_name f == [smt|and|] = xs
conjuncts x = [x]

disjuncts :: (IsName n,TypeSystem2 t) => AbsExpr n t q -> [AbsExpr n t q]
disjuncts (FunApp f xs)
    | z3_name f == [smt|or|] = xs
    --  name f == "=>"  = map znot (take 1 xs) ++ drop 1 xs
disjuncts x = [x]

applyAssoc :: (Int -> AbsFun n a) -> [GenExpr n t a q] -> GenExpr n t a q
applyAssoc op xs = funApp (op (length xs)) xs

flattenConnectors :: (IsName n,TypeSystem2 t,IsQuantifier q) 
                  => AbsExpr n t q -> AbsExpr n t q
flattenConnectors = f . rewrite flattenConnectors
    where
        f e@(FunApp fn xs) 
            | z3_name fn == [smt|or|]  = applyAssoc or_fun $ concatMap disjuncts xs
            | z3_name fn == [smt|and|] = applyAssoc and_fun $ concatMap conjuncts xs
            | otherwise                = e
        f e = e

zlift :: t -> AbsExpr n t q -> AbsExpr n t q
zlift t e = Lift e t

const_fun :: IsName n => AbsFun n Type
const_fun = mk_fun [gA,gB] [smt|const|] [gB] (array gA gB)

zconst :: ExprP -> ExprP
zconst = typ_fun1 const_fun

isDef_fun :: IsName n => AbsFun n Type
isDef_fun = mk_fun [gA] [smt|is-def|] [guarded_type gA] bool

ident_fun :: IsName n => AbsFun n Type
ident_fun = mk_fun [gA] [smt|ident|] [] (array gA gA)

elem_fun :: (IsName n,TypeSystem t)
         => t -> AbsFun n t
elem_fun t = mk_fun' [t] "elem" [t,set_type t] bool

zIsDef :: ExprP -> ExprP
zIsDef = typ_fun1 isDef_fun

zelem         :: (IsQuantifier q,IsName n) 
              => ExprPG n Type q -> ExprPG n Type q -> ExprPG n Type q
zelem        = typ_fun2 (elem_fun gA)

zelem'        :: UntypedExpr -> UntypedExpr -> UntypedExpr
zelem'       = fun2' (elem_fun gA)


zelemUnchecked :: (TypeSystem t,IsName n,IsQuantifier q)
               => AbsExpr n t q 
               -> AbsExpr n t q 
               -> AbsExpr n t q
zelemUnchecked x s = provided (set_type (type_of x) == type_of s) $
        funApp (elem_fun (type_of x)) [x,s]

zident :: ExprP
zident = Right $ funApp ident_fun []

zrecord' :: (TypeSystem t,IsName n,IsQuantifier q) 
         => MapSyntax Field (ExprPG n t q) () 
         -> ExprPG n t q
zrecord' = zrecord . runMap'
zrecord :: (TypeSystem t,IsName n,IsQuantifier q)
        => M.HashMap Field (ExprPG n t q) 
        -> ExprPG n t q
zrecord m = do
        m' <- traverseValidation id m
        let t = record_type (type_of <$> m')
        return $ Record (RecLit m') t

zrec_update :: (TypeSystem t,IsName n,IsQuantifier q)
            => ExprPG n t q 
            -> MapSyntax Field (ExprPG n t q) ()
            -> ExprPG n t q
zrec_update e = zrec_update' e . runMap'

zrec_update' :: (TypeSystem t,IsName n,IsQuantifier q)
             => ExprPG n t q 
             -> M.HashMap Field (ExprPG n t q)
             -> ExprPG n t q
zrec_update' e m = do
    let f e = case type_of e^?fieldTypes of
                Just t -> Right (e,t)
                _ -> Left [[st|expecting a record type, met %s in %s|] 
                            (prettyText $ type_of e)
                            (prettyText e)
                            ]
    (e',t') <- f =<< e
    m' <- traverseValidation id m
    return $ Record (RecUpdate e' m') (record_type $ (type_of <$> m') `M.union` t')

zfield :: (IsName n,TypeSystem t,IsQuantifier q) 
       => ExprPG n t q -> Field -> ExprPG n t q
zfield e field = do
    -- let field = Field field'
    e' <- e
    let msg1 = [st|Field lookup requires a record type:\n  in expression %s\n  of type: %s|] 
                    (prettyText e')
                    (prettyText $ type_of e')
    fs <- maybe (Left [msg1]) Right 
        $ type_of e'^?fieldTypes
    let msg2 = [st|Type of expression does not have field '%s':\n  in expression %s\n  with fields: %s|]
                    (prettyText field)
                    (prettyText e')
                    (prettyText $ M.keys fs)
    t <- maybe (Left [msg2]) Right
        $ M.lookup field fs 
    return $ Record (FieldLookup e' field) t

instance Num ExprP where
    (-) = mzminus
    (+) = mzplus
    (*) = mztimes
    abs = typ_fun1 $ mk_fun [] [smt|abs|] [int] int
    signum x = zite (x .< 0) (-1) $ zite (0 .< x) 1 0
    fromInteger = mzint . fromInteger
    negate = mzopp

