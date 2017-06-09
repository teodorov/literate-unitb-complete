{-# LANGUAGE TypeOperators
        , QuasiQuotes
        , StandaloneDeriving
        , CPP
        , ConstraintKinds
        , RankNTypes
        , TemplateHaskell
        , DeriveGeneric
        , DeriveTraversable
        , TypeFamilies
        , GeneralizedNewtypeDeriving
        , KindSignatures
        , TypeSynonymInstances
        , FlexibleInstances
        , FlexibleContexts
        , DeriveFunctor
        , ScopedTypeVariables
        , MultiParamTypeClasses
        , DefaultSignatures #-}
module GHC.Generics.Instances 
    ( Generic, genericLift, genericMEmpty, genericMAppend
    , genericMConcat, genericDefault, genericSemigroupMAppend
    , Intersection(..), genericSemigroupMAppendWith
    , genericSemigroupMConcat, genericSemigroupMConcatWith
    , show1, shows1, NFData1(..), deepseq1
    , Serialize1(..)
    , genericArbitrary, inductive, listOf', arbitrary' 
    , Lift1(..), Monoid1(..)
    , Default1(..)
    , Compose(..)
#if MIN_VERSION_transformers(0,5,0)
    , genericLiftEq
    , genericLiftCompare
    , genericLiftShowsPrec
    , genericLiftReadsPrec
#endif
    , arbitraryCompose
    , OnFunctor(..) )
where

import Control.DeepSeq
import           Control.Monad.Fix
#if MIN_VERSION_transformers(0,5,0)
import           Control.Monad
import           Control.Monad.State (StateT(..),state)
#endif
#if MIN_VERSION_transformers_compat(0,5,0)
import Control.Monad.Trans.Instances ()
#else
#endif
import Control.Lens

import Data.Default
import Data.Either.Validation

#if MIN_VERSION_transformers(0,5,0)
import Prelude.Extras hiding (Lift1)
import           Data.Functor.Classes hiding (Eq1,Ord1,Show1,Read1)
import qualified Data.Functor.Classes as F
#else
import Data.Functor.Classes
#endif
import Data.Functor.Compose
import Data.Hashable
import Data.DList (DList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy
import Data.Proxy.TH
import Data.Serialize (Serialize)
import Data.Serialize.Instances
import qualified Data.Serialize as S
import Data.Set (Set)
import qualified Data.Set as S
import Data.Semigroup
import Data.Tuple.Generics

import GHC.Generics.Utils

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Test.QuickCheck

class GMonoid a where
    gmempty :: a p
    gmappend :: a p -> a p -> a p
    gmconcat :: [a p] -> a p

instance GMonoid c => GMonoid (M1 a b c) where
    gmempty  = M1 gmempty
    gmappend (M1 x) (M1 y) = M1 $ gmappend x y
    gmconcat xs = M1 $ gmconcat $ map unM1 xs

instance Monoid b => GMonoid (K1 a b) where
    gmempty = K1 mempty
    gmappend (K1 x) (K1 y) = K1 $ mappend x y
    gmconcat xs = K1 $ mconcat $ map unK1 xs

instance (GMonoid a,GMonoid b) => GMonoid (a :*: b) where
    gmempty = gmempty :*: gmempty
    gmappend (x0 :*: x1) (y0 :*: y1) = gmappend x0 y0 :*: gmappend x1 y1
    gmconcat xs = gmconcat (map f xs) :*: gmconcat (map g xs)
        where
            f (x :*: _) = x
            g (_ :*: x) = x

class GSemigroupWith a where
    gSemiMAppend :: a p -> a p -> a p
    gSemiMConcat :: NonEmpty (a p) -> a p

instance (GSemigroupWith c) => GSemigroupWith (M1 a b c) where
    gSemiMAppend x y = M1 (gSemiMAppend (unM1 x) (unM1 y))
    gSemiMConcat xs  = M1 (gSemiMConcat $ unM1 <$> xs)

instance Semigroup b => GSemigroupWith (K1 a b) where
    gSemiMAppend x y = K1 (unK1 x <> unK1 y)
    gSemiMConcat xs  = K1 (sconcat $ unK1 <$> xs)

instance (GSemigroupWith a,GSemigroupWith b) 
        => GSemigroupWith (a :*: b) where
    gSemiMAppend x y = gSemiMAppend (x^.left) (y^.left) :*:
                             gSemiMAppend (x^.right) (y^.right)
    gSemiMConcat xs = gSemiMConcat (view left <$> xs) :*: gSemiMConcat (view right <$> xs)

class Applicative f => MapFields a (f :: * -> *) where
    type Mapped a f :: * -> *
    put :: f (a p) -> Mapped a f p
    get :: Mapped a f p -> f (a p)

instance MapFields c f => MapFields (M1 a b c) f where
    type Mapped (M1 a b c) f = M1 a b (Mapped c f)
    put x = M1 $ put (unM1 <$> x)
    get x = M1 <$> get (unM1 x)

instance (Applicative f) => MapFields (K1 a b) f where
    type Mapped (K1 a b) f = K1 a (f b)
    put = K1 . fmap unK1
    get = fmap K1 . unK1

instance (MapFields a f,MapFields b f) => MapFields (a :*: b) f where
    type Mapped (a :*: b) f = Mapped a f :*: Mapped b f
    put x = put (view left <$> x) :*: put (view right <$> x)
    get x = (:*:) <$> get (x^.left) <*> get (x^.right)


genericMEmpty :: (Generic a, GMonoid (Rep a)) => a
genericMEmpty = gmempty^.from generic
genericMAppend :: (Generic a, GMonoid (Rep a)) => a -> a -> a
genericMAppend x y = gmappend (x^.generic) (y^.generic)^.from generic
genericMConcat :: (Generic a, GMonoid (Rep a)) => [a] -> a
genericMConcat xs = gmconcat (map (view generic) xs)^.from generic

class Monoid1 (f :: * -> *) where
    mempty1 :: f a
    default mempty1 :: Monoid (f a) => f a
    mempty1 = mempty
    mappend1 :: f a -> f a -> f a
    default mappend1 :: Monoid (f a) => f a -> f a -> f a
    mappend1 = mappend
    mconcat1 :: [f a] -> f a
    default mconcat1 :: Monoid (f a) => [f a] -> f a
    mconcat1 = mconcat

instance Monoid1 f => Monoid (OnFunctor f a) where
    mempty  = OnFunctor mempty1
    mappend (OnFunctor x) (OnFunctor y) = OnFunctor $ x `mappend1` y
    mconcat xs = OnFunctor $ mconcat1 $ map getFunctor xs

instance Monoid1 [] where
instance Monoid1 DList where
instance Ord k => Monoid1 (Map k) where

genericSemigroupMAppend :: (Generic a, GSemigroupWith (Rep a)) => a -> a -> a
genericSemigroupMAppend x y = gSemiMAppend (x^.generic) (y^.generic)^.from generic

genericSemigroupMConcat :: (Generic a, GSemigroupWith (Rep a)) => NonEmpty a -> a
genericSemigroupMConcat xs = gSemiMConcat (view generic <$> xs)^.from generic

genericSemigroupMAppendWith :: forall a f. (Functor f,Generic a,MapFields (Rep a) f,GSemigroupWith (Mapped (Rep a) f)) 
                            => f a -> f a -> f a
genericSemigroupMAppendWith x y = view (from generic) <$> get (gSemiMAppend (put $ view generic <$> x) (put $ view generic <$> y))

genericSemigroupMConcatWith :: forall a f. (Functor f,Generic a,MapFields (Rep a) f,GSemigroupWith (Mapped (Rep a) f)) 
                            => NonEmpty (f a) -> f a
genericSemigroupMConcatWith xs = view (from generic) <$> get (gSemiMConcat $ put.fmap (view generic) <$> xs)

newtype Intersection a = Intersection { getIntersection :: a }
    deriving (Functor)

instance Applicative Intersection where
    pure = Intersection
    Intersection f <*> Intersection x = Intersection $ f x

instance Ord k => Semigroup (Intersection (Map k a)) where
    Intersection x <> Intersection y = Intersection $ x `M.intersection` y

instance Ord k => Semigroup (Intersection (Set k)) where
    Intersection x <> Intersection y = Intersection $ x `S.intersection` y



class Default1 f where
    def1 :: Default a => f a

instance (Functor f,Default1 f,Default1 g,Default x) => Default (Compose f g x) where
    def = Compose $ getFunctor <$> def1



instance (Default x,Default1 f) => Default (OnFunctor f x) where
    def = OnFunctor def1


makeTuple'' :: (Generic a, GIsTuple constr (Rep a),Applicative f) 
            => Proxy constr 
            -> (forall b. constr b => Proxy b -> f b)
            -> Proxy a
            -> f a
makeTuple'' p f x = view (from generic) <$> gMakeTuple p f (view generic <$> x)

genericDefault :: (Generic a, GIsTuple Default (Rep a)) => a
genericDefault = runIdentity $ makeTuple'' [pr|Default|] (const $ Identity def) Proxy

class GArbitrary a where
    gArbitrary :: [Gen (a p)]

instance GArbitrary c => GArbitrary (M1 a b c) where
    gArbitrary = fmap M1 <$> gArbitrary

instance Arbitrary b => GArbitrary (K1 a b) where
    gArbitrary = [K1 <$> arbitrary]

instance (GArbitrary a,GArbitrary b) => GArbitrary (a :*: b) where
    gArbitrary = getCompose $ (:*:) <$> Compose gArbitrary <*> Compose gArbitrary

instance (GArbitrary a,GArbitrary b) => GArbitrary (a :+: b) where
    gArbitrary = (fmap L1 <$> gArbitrary) ++ (fmap R1 <$> gArbitrary)

instance GArbitrary U1 where
    gArbitrary = [return U1]

genericArbitrary :: (Generic a, GArbitrary (Rep a)) => Gen a
genericArbitrary = view (from generic) <$> oneof gArbitrary

class GLifts a => GLift a where
    glift :: a p -> ExpQ

class GLifts a where
    glifts :: a p -> [ExpQ]
    default glifts :: GLift a => a p -> [ExpQ]
    glifts x = [glift x]

instance GLift b => GLifts (D1 a b) where
instance GLift b => GLift (D1 a b) where
    glift (M1 x) = glift x

instance Lift b => GLifts (K1 a b) where
instance Lift b => GLift (K1 a b) where
    glift (K1 x) = lift x

instance GLift b => GLifts (S1 s b) where
instance GLift b => GLift (S1 s b) where
    glift (M1 x) = glift x

instance (Constructor c,GLifts b) => GLifts (C1 c b) where
instance (Constructor c,GLifts b) => GLift (C1 c b) where
    glift c@(M1 x) = appsE $ conE (mkName $ conName c) : glifts x

instance (GLift a, GLift b) => GLifts (a :+: b) where
instance (GLift a, GLift b) => GLift (a :+: b) where
    glift (L1 x) = glift x
    glift (R1 x) = glift x

instance GLifts U1 where
    glifts U1 = []

instance (GLifts a, GLifts b) => GLifts (a :*: b) where
    glifts (x :*: y) = glifts x ++ glifts y

genericLift :: (Generic a, GLift (Rep a)) => a -> ExpQ
genericLift = glift . view generic

instance Lift a => Lift (NonEmpty a) where
    lift = genericLift

instance (Lift k,Lift a,Ord k) => Lift (Map k a) where
    lift m = [e| M.fromList $(listE $ lift <$> M.toList m) |]

instance (Lift a,Ord a) => Lift (Set a) where
    lift m = [e| S.fromList $(listE $ lift <$> S.toList m) |]

inductive :: (Compose Maybe Gen a -> [Compose Maybe Gen a]) -> Gen a
inductive f = sized $ fix $ \ind n -> oneof =<< catMaybes . map getCompose . f <$> cmd ind n
    where
        cmd :: (Int -> Gen a) -> Int -> Gen (Compose Maybe Gen a)
        cmd r n =
                if n == 0 then return $ Compose Nothing
                          else return $ Compose $ Just $ r (n `div` 10)

listOf' :: Compose Maybe Gen a -> Compose Maybe Gen [a]
listOf' (Compose cmd) = Compose $ listOf <$> cmd

arbitrary' :: Arbitrary a => Compose Maybe Gen a
arbitrary' = Compose $ Just arbitrary

#if MIN_VERSION_base(4,9,0)
#else
instance Semigroup (DList a) where
#endif

#if MIN_VERSION_transformers(0,5,0)
#else
instance Eq1 Proxy where
    eq1 = (==)

instance Show1 Proxy where
    showsPrec1 = showsPrec
#endif
instance Eq1 NonEmpty where
#if MIN_VERSION_transformers(0,5,0)
    (==#) = (==)
#else
    eq1 = (==)
#endif

instance Ord1 NonEmpty where
    compare1 = compare

instance Show1 NonEmpty where
    showsPrec1 = showsPrec

instance (Show1 f,Show a) => Show (OnFunctor f a) where
    show = show1 . getFunctor

#if MIN_VERSION_transformers(0,5,0)
#else
show1 :: (Show a, Show1 f) => f a -> String
show1 x = showsPrec1 0 x ""

shows1 :: (Show a, Show1 f) => f a -> ShowS
shows1 = showsPrec1 0
#endif

class NFData1 f where
    rnf1 :: NFData a => f a -> ()

deepseq1 :: (NFData a, NFData1 f) => f a -> b -> b
deepseq1 x y = rnf1 x `seq` y

instance NFData1 Proxy where
    rnf1 = rnf
instance NFData a => NFData1 (Const a) where
    rnf1 = rnf
instance NFData1 Identity where
    rnf1 = rnf
instance NFData1 [] where
    rnf1 = rnf
instance NFData1 NonEmpty where
    rnf1 = rnf
instance (Functor f,NFData1 f,NFData1 g) => NFData1 (Compose f g) where
    rnf1 = rnf . OnFunctor . fmap OnFunctor . getCompose
instance NFData a => NFData1 ((,) a) where
    rnf1 = rnf
instance NFData1 Maybe where
    rnf1 = rnf

newtype OnFunctor f a = OnFunctor { getFunctor :: (f a) }
    deriving (Functor,Applicative,Monad,Traversable,Foldable)

instance Rewrapped (OnFunctor f a) (OnFunctor g b) where

instance Wrapped (OnFunctor f a) where
    type Unwrapped (OnFunctor f a) = f a
    _Wrapped' = iso getFunctor OnFunctor

instance (NFData a,NFData1 f) => NFData (OnFunctor f a) where
    rnf = rnf1 . getFunctor

-- #if MIN_VERSION_transformers(0,5,0)
-- #else
class Lift1 f where
    lift1 :: Lift a => f a -> ExpQ
-- #endif

instance Lift a => Lift (Const a b) where
    lift (Const x) = [e| Const $(lift x) |]
instance Lift a => Lift (Identity a) where
    lift (Identity x) = [e| Identity $(lift x) |]
instance Lift a => Lift1 (Const a) where
    lift1 = lift
instance Lift1 Identity where
    lift1 = lift
instance Lift1 [] where
    lift1 = lift
instance Lift1 NonEmpty where
    lift1 = lift
instance (Lift1 f,Lift1 g,Functor f) => Lift1 (Compose f g) where
    lift1 (Compose x) = lift1 $ OnFunctor <$> x
instance (Lift1 f,Lift a) => Lift (OnFunctor f a) where
    lift (OnFunctor x) = lift1 x
instance Lift a => Lift1 ((,) a) where
    lift1 = lift
instance Lift1 Maybe where
    lift1 = lift

#if MIN_VERSION_transformers_compat(0,5,0)
#else
deriving instance Generic (Compose f g a)
#endif
deriving instance Generic (Validation a b)

arbitraryCompose :: Arbitrary (f (g a)) => Gen (Compose f g a)
arbitraryCompose = Compose <$> arbitrary

instance (NFData a,NFData b) => NFData (Validation a b) where

instance Serialize1 Proxy where
instance Serialize1 Identity where
instance Serialize1 Maybe where

instance Serialize (Proxy a) where

instance Serialize (f (g a)) 
        => Serialize (Compose f g a) where
instance Serialize a => Serialize (Identity a) where

#if MIN_VERSION_QuickCheck(2,9,0)
#else
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary
    shrink = genericShrink
#endif
instance Arbitrary (Proxy a) where
    arbitrary = return Proxy

instance (Serialize a,Serialize1 f) => Serialize (OnFunctor f a) where
    put = put1 . getFunctor
    get = OnFunctor <$> get1

instance (Hashable k,Hashable a) => Hashable (Map k a) where
    hashWithSalt salt = hashWithSalt salt . M.toList

instance Hashable a => Hashable (Set a) where
    hashWithSalt salt = hashWithSalt salt . S.toList

#if MIN_VERSION_transformers(0,5,0)

instance F.Eq1 f => F.Eq1 (M1 x c f) where
    liftEq f (M1 x) (M1 y) = liftEq f x y
instance Eq c => F.Eq1 (K1 x c) where
    liftEq _ (K1 x) (K1 y) = x == y
instance (F.Eq1 f,F.Eq1 g) => F.Eq1 (f :+: g) where
    liftEq f (L1 x) (L1 y) = liftEq f x y
    liftEq _ (L1 _) (R1 _) = False
    liftEq f (R1 x) (R1 y) = liftEq f x y
    liftEq _ (R1 _) (L1 _) = False
instance (F.Eq1 f) => F.Eq1 (Rec1 f) where
    liftEq f (Rec1 x) (Rec1 y) = liftEq f x y
instance F.Eq1 Par1 where
    liftEq f (Par1 x) (Par1 y) = f x y
instance (F.Eq1 f,F.Eq1 g) => F.Eq1 (f :.: g) where
    liftEq f (Comp1 x) (Comp1 y) = liftEq (liftEq f) x y
instance (F.Eq1 f,F.Eq1 g) => F.Eq1 (f :*: g) where
    liftEq f (x0 :*: x1) (y0 :*: y1) = liftEq f x0 y0 && liftEq f x1 y1

genericLiftEq :: (Generic1 f, F.Eq1 (Rep1 f)) 
              => (a -> b -> Bool)
              -> f a -> f b -> Bool
genericLiftEq f x y = liftEq f (from1 x) (from1 y)

instance F.Ord1 f => F.Ord1 (M1 x c f) where
    liftCompare f (M1 x) (M1 y) = liftCompare f x y
instance Ord c => F.Ord1 (K1 x c) where
    liftCompare _ (K1 x) (K1 y) = compare x y
instance (F.Ord1 f,F.Ord1 g) => F.Ord1 (f :+: g) where
    liftCompare f (L1 x) (L1 y) = liftCompare f x y
    liftCompare _ (L1 _) (R1 _) = LT
    liftCompare f (R1 x) (R1 y) = liftCompare f x y
    liftCompare _ (R1 _) (L1 _) = GT
instance (F.Ord1 f) => F.Ord1 (Rec1 f) where
    liftCompare f (Rec1 x) (Rec1 y) = liftCompare f x y
instance F.Ord1 Par1 where
    liftCompare f (Par1 x) (Par1 y) = f x y
instance (F.Ord1 f,F.Ord1 g) => F.Ord1 (f :.: g) where
    liftCompare f (Comp1 x) (Comp1 y) = liftCompare (liftCompare f) x y
instance (F.Ord1 f,F.Ord1 g) => F.Ord1 (f :*: g) where
    liftCompare f (x0 :*: x1) (y0 :*: y1) = liftCompare f x0 y0 <> liftCompare f x1 y1

genericLiftCompare :: (Generic1 f, F.Ord1 (Rep1 f)) 
              => (a -> b -> Ordering)
              -> f a -> f b -> Ordering
genericLiftCompare f x y = liftCompare f (from1 x) (from1 y)

withR :: (a -> b) -> ReadS a -> ReadS b
withR f = mapped.mapped._1 %~ f

instance F.Read1 f => F.Read1 (S1 c f) where
    liftReadsPrec f g n = runStateT $ do
        x <- state (splitAt 1)
        guard (x == " ")
        M1 <$> StateT (liftReadsPrec f g n)
instance (F.Read1 f,Constructor c) => F.Read1 (C1 c f) where
    liftReadsPrec f g n = readParen (n > 10) $ runStateT $ do
        kw <- StateT lex
        x <- M1 <$> StateT (liftReadsPrec f g n)
        guard (kw == conName x)
        return x
instance F.Read1 f => F.Read1 (D1 c f) where
    liftReadsPrec f g n = withR M1 $ liftReadsPrec f g n
instance (F.Read1 f,F.Read1 g) => F.Read1 (f :+: g) where
    liftReadsPrec f g n = withR L1 (liftReadsPrec f g n) <> withR R1 (liftReadsPrec f g n)
instance (F.Read1 f) => F.Read1 (Rec1 f) where
    liftReadsPrec f g n = withR Rec1 $ liftReadsPrec f g n
instance F.Read1 Par1 where
    liftReadsPrec f _ n = withR Par1 (f n)
instance (F.Read1 f,F.Read1 g) => F.Read1 (f :.: g) where
    liftReadsPrec f g n = withR Comp1 $ liftReadsPrec (liftReadsPrec f g) (liftReadList f g) n
instance (F.Read1 f,F.Read1 g) => F.Read1 (f :*: g) where
    liftReadsPrec f g n xs0 = do
        (x0,xs1) <- liftReadsPrec f g n xs0
        (x1,xs2) <- liftReadsPrec f g n xs1
        return (x0 :*: x1,xs2)

genericLiftShowsPrec :: (Generic1 f, F.Show1 (Rep1 f)) 
              => (Int -> a -> ShowS)
              -> ([a] -> ShowS)
              -> Int
              -> f a -> ShowS
genericLiftShowsPrec f g n x = liftShowsPrec f g n (from1 x)

instance (F.Show1 f,Constructor c) => F.Show1 (C1 c f) where
    liftShowsPrec f g n c@(M1 x) = showParen (n > 10) $
        showString (conName c) . showChar ' ' . liftShowsPrec f g n x
instance F.Show1 f => F.Show1 (D1 c f) where
    liftShowsPrec f g n (M1 x) = liftShowsPrec f g n x
instance Show a => F.Show1 (K1 c a) where
    liftShowsPrec _ _ = lmap unK1 . showsPrec
instance F.Show1 f => F.Show1 (S1 c f) where
    liftShowsPrec f g n (M1 x) = showChar ' ' . liftShowsPrec f g n x
instance (F.Show1 f,F.Show1 g) => F.Show1 (f :+: g) where
    liftShowsPrec f g n (L1 x) = liftShowsPrec f g n x
    liftShowsPrec f g n (R1 x) = liftShowsPrec f g n x
instance (F.Show1 f) => F.Show1 (Rec1 f) where
    liftShowsPrec f g n (Rec1 x) = liftShowsPrec f g n x
instance F.Show1 Par1 where
    liftShowsPrec f _ n (Par1 x) = f n x
instance (F.Show1 f,F.Show1 g) => F.Show1 (f :.: g) where
    liftShowsPrec f g n (Comp1 x) = liftShowsPrec (liftShowsPrec f g) (liftShowList f g) n x
instance (F.Show1 f,F.Show1 g) => F.Show1 (f :*: g) where
    liftShowsPrec f g _ (x0 :*: x1) = liftShowsPrec f g 11 x0 . liftShowsPrec f g 11 x1

genericLiftReadsPrec :: (Generic1 f, F.Read1 (Rep1 f)) 
              => (Int -> ReadS a)
              -> ReadS [a]
              -> Int
              -> ReadS (f a)
genericLiftReadsPrec f g n = withR to1 $ liftReadsPrec f g n


instance Ord k => F.Eq1 (Map k) where
    liftEq f m0 m1 = M.null $ M.mergeWithKey (\_ x y -> guard $ not $ f x y) (() <$) (() <$) m0 m1
instance Ord k => F.Ord1 (Map k) where
    liftCompare f m0 m1 = foldMap id $ M.mergeWithKey (\_ x y -> Just $ f x y) (GT <$) (LT <$) m0 m1

instance Show k => F.Show1 (Map k) where
    -- liftShowsPrec showA showAs n = liftShowsPrec 
    --             (liftShowsPrec showA showAs) 
    --             (liftShowList showA showAs) n 
    --         . M.toList
    liftShowsPrec _ _ _ _ = id

#endif