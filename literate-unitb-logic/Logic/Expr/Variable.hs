{-# LANGUAGE TypeFamilies #-}
module Logic.Expr.Variable where

    -- Module
import Logic.Expr.Classes as Expr
import Logic.Expr.PrettyPrint
import Logic.Expr.Type
import Logic.Names

    -- Library
import Control.DeepSeq
import Control.Lens hiding (rewrite,Context
                           ,Const,Context'
                           ,Traversable1(..))
import Control.Precondition

import Data.Data
import Data.Hashable
import Data.HashMap.Lazy as M
import Data.HashMap.Lazy.Extras as M
import Data.Serialize
import           Data.Text (Text)

import GHC.Generics.Instances

import Language.Haskell.TH.Syntax hiding (Name,Type)

import Test.QuickCheck
import Test.QuickCheck.ZoomEq

import Utilities.Functor

type UntypedVar = AbsVar Name ()

type Var = AbsVar Name GenericType

type Var' = AbsVar InternalName Type

type FOVar = AbsVar InternalName FOType

data AbsVar name t = Var !name !t
    deriving (Eq,Ord,Generic,Typeable,Data,Functor,Foldable,Traversable,Show)

translate' :: (n0 -> n1) -> AbsVar n0 t -> AbsVar n1 t
translate' = fmap1

instance (Hashable name,Hashable t) => Hashable (AbsVar name t) where

instance (ZoomEq t,ZoomEq n) => ZoomEq (AbsVar n t) where

instance (Arbitrary t,Arbitrary n) => Arbitrary (AbsVar n t) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance IsName n => Translatable (AbsVar n t) (AbsVar InternalName t) where
    translate = translate' asInternal

instance (TypeSystem t,IsName n) => Tree (AbsVar n t) where
    as_tree' (Var vn vt) = do
        t <- as_tree' vt
        return $ Expr.List [Str $ renderText vn, t]
    rewriteM _ = pure

instance (TypeSystem t) => Typed (AbsVar n t) where
    type TypeOf (AbsVar n t) = t
    type_of (Var _ t) = t

instance (TypeSystem t, IsName n) => PrettyPrintable (AbsVar n t) where
    pretty (Var n t) = render n ++ ": " ++ show (as_tree t)

instance Functor1 AbsVar where
instance Foldable1 AbsVar where
instance Traversable1 AbsVar where
    traverseOn1 f g (Var n t) = Var <$> f n <*> g t

prime :: IsName n => AbsVar n t -> AbsVar n t
prime (Var n t) = Var (addPrime n) t

primeAll :: IsName n => HashMap n (AbsVar n t) -> HashMap n (AbsVar n t)
primeAll m = M.mapKeys addPrime $ M.map prime m

z3Var :: Pre
      => Text -> t -> AbsVar Name t
z3Var = Var . fromText

instance HasName (AbsVar n t) n where
    name = to $ \(Var x _) -> x

instance HasNames (AbsVar n t) n where
    type SetNameT n' (AbsVar n t) = AbsVar n' t
    namesOf f (Var x t) = Var <$> f x <*> pure t

instance (IsName n,Typeable t) => Named (AbsVar n t) where
    type NameOf (AbsVar n t) = n
    decorated_name' (Var x _) = adaptName x

instance (Serialize n,Serialize t) => Serialize (AbsVar n t) where

instance (NFData t,NFData n) => NFData (AbsVar n t)

instance (Lift n,Lift t) => Lift (AbsVar n t) where
    lift = genericLift
