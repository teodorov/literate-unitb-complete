{-# LANGUAGE TypeFamilies,TemplateHaskell #-}
module Logic.Expr.Fun where

    -- Module
import Logic.Expr.Classes as Expr
import Logic.Expr.PrettyPrint
import Logic.Expr.Type
import Logic.Names

    -- Library
import Control.DeepSeq
import Control.Lens hiding (rewrite,Context,elements
                           ,Const,Context',rewriteM
                           ,Traversable1(..))
import Control.Precondition

import           Data.Data
import           Data.Hashable
import           Data.List as L
import           Data.Serialize

import GHC.Generics.Instances

import Language.Haskell.TH.Syntax hiding (Name,Type)

import Test.QuickCheck
import Test.QuickCheck.ZoomEq

import Utilities.Functor

data SetWD  = FiniteWD | InfiniteWD
    deriving (Eq,Ord,Generic,Typeable,Data,Show)

data AbsFun n t = Fun 
        { _annotation :: ![t]
        , _funName :: !n
        , lifted :: !Lifting
        , _argumentTypes :: ![t] 
        , _resultType :: !t
        , _finite :: !SetWD }
    deriving (Eq,Ord,Generic,Typeable,Data,Functor,Foldable,Traversable,Show)

data Lifting = Unlifted | Lifted
    deriving (Eq,Ord, Generic, Data, Typeable,Show)

makeLenses ''AbsFun
makeFields ''AbsFun

instance Serialize Lifting where

instance (Hashable n,Hashable t) => Hashable (AbsFun n t)
instance Hashable Lifting
instance Hashable SetWD

instance ZoomEq Lifting where
instance Arbitrary Lifting where
    arbitrary = genericArbitrary

instance ZoomEq SetWD where
instance Arbitrary SetWD where
    arbitrary = genericArbitrary

instance (ZoomEq t,ZoomEq n) => ZoomEq (AbsFun n t) where
instance (Arbitrary t,Arbitrary n) => Arbitrary (AbsFun n t) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (NFData t,NFData n) => NFData (AbsFun n t) where
instance NFData Lifting
instance NFData SetWD

instance Functor1 AbsFun where
instance Foldable1 AbsFun where
instance Traversable1 AbsFun where
    traverseOn1 f g (Fun xs n l ts t w) = 
        Fun <$> traverse g xs
            <*> f n
            <*> pure l
            <*> traverse g ts
            <*> g t
            <*> pure w

instance (PrettyPrintable n,PrettyPrintable t,Tree t) => PrettyPrintable (AbsFun n t) where
    pretty (Fun xs n _ ts t _) = pretty n ++ pretty xs ++ ": " 
            ++ args ++ pretty (as_tree t)
        where
            args
                | not $ null ts = intercalate " x " (map (pretty . as_tree) ts) ++ " -> "
                | otherwise     = ""

instance (TypeSystem t) => Typed (AbsFun n t) where
    type TypeOf (AbsFun n t)  = t
    type_of (Fun _ _ _ _ t _) = t

instance HasName (AbsFun n t) n where
    name = to $ \(Fun _ x _ _ _ _) -> x

instance HasNames (AbsFun n t) n where
    type SetNameT n' (AbsFun n t) = AbsFun n' t
    namesOf = traverse1

instance (IsName n,TypeSystem t) => Named (AbsFun n t) where
    type NameOf (AbsFun n t) = n
    decorated_name' (Fun ts x _ _ _ _) = do
            ts' <- mapM z3_decoration' ts
            let suf = concat ts'
            onInternalName (addSuffix suf) 
                $ adaptName x

mk_fun' :: (Pre,IsName n) 
        => [t] -> String -> [t] -> t -> AbsFun n t
mk_fun' ps = mk_fun ps . z3Name

mk_fun :: [t] -> n -> [t] -> t -> AbsFun n t
mk_fun  ps n ts t = Fun (evalList ps) n Unlifted (evalList ts) t InfiniteWD

mk_lifted_fun :: [t] -> n -> [t] -> t -> AbsFun n t
mk_lifted_fun ps n ts t = Fun (evalList ps) n Lifted (evalList ts) t InfiniteWD

mkConstant :: (Pre,IsName n) 
           => String -> t -> AbsFun n t
mkConstant n t = mk_fun [] (fromString'' n) [] t

    -- replace it everywhere (replace what? with what?)
z3_fun_name :: Fun -> InternalName
z3_fun_name (Fun xs ys _ _ _ _) = fromString'' $ render ys ++ concatMap z3_decoration xs

isLifted :: AbsFun n t -> Bool
isLifted (Fun _ _ lf _ _ _) = lf == Lifted

type Fun = AbsFun Name GenericType

type FOFun = AbsFun InternalName FOType

type InternalFun = AbsFun InternalName Type

instance (Data n,Data t,IsName n,TypeSystem t) => Tree (AbsFun n t) where
    as_tree' f@(Fun _ _ _ argT rT _) = Expr.List <$> sequenceA
            [ Str  <$> render_decorated f
            , Expr.List <$> mapM as_tree' argT 
            , as_tree' rT ]

instance (Serialize n,Serialize t) => Serialize (AbsFun n t) where
instance Serialize SetWD where

instance (Lift n,Lift a) => Lift (AbsFun n a) where
    lift = genericLift

instance Lift SetWD where
    lift = genericLift

instance Lift Lifting where
    lift = genericLift

