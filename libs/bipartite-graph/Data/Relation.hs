{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TypeOperators,CPP  #-}
module Data.Relation where

    -- Libraries
import Control.Arrow
import Control.DeepSeq
import Control.Monad

import Data.Default
import qualified Data.Graph.Array as Perm
import Data.List hiding (union,transpose,null)
import qualified Data.List.Ordered as LO
import Data.Monoid
import Data.Tuple
import qualified Data.HashMap.Lazy as M
import           Data.HashMap.Lazy.Extras as M (Key)
import qualified Data.HashMap.Lazy.Extras as M
import Data.Maybe
import qualified Data.HashSet as S

import GHC.Generics

import Prelude hiding (null)

import Test.QuickCheck
import Test.QuickCheck.Function
import Test.QuickCheck.Instances ()
import Test.QuickCheck.Report

infixr 6 <|
infixr 6 <<|
infixl 7 |>
infixl 7 |>>
infix <->

newtype Relation a b = Rel (M.HashMap a (M.HashMap b ()))
    deriving (Eq,Default,Generic)

type (<->) a b = Relation a b

instance (Show a, Show b) => Show (Relation a b) where
    show r = "fromList " ++ show (toList r)

instance (Key a,Key b) 
        => Monoid (Relation a b) where
    mempty = empty
    mappend = union

toList :: Relation a b -> [(a,b)]
toList (Rel m) = [ (x,y) | (x,xs) <- M.toList m, (y,()) <- M.toList xs ]

fromList :: (Key a,Key b) => [(a,b)] -> Relation a b
fromList xs = Rel $ M.map M.fromList $ M.fromListWith (++) [ (x,[(y,())]) | (x,y) <- xs ]

fromListMap :: (Key a,Key b) => M.HashMap a [b] -> Relation a b
fromListMap m = Rel $ M.map (M.fromList . map pair) m
    where
        pair x = (x,())

empty :: Relation a b
empty = Rel M.empty

compose :: (Key a,Key b,Key c) 
        => Relation a b -> Relation b c -> Relation a c
compose (Rel r0) (Rel r1) = Rel $ cleanup $ M.map (M.unions . M.elems . (r1 `M.intersection`)) r0

cleanup :: M.HashMap k0 (M.HashMap k1 a) -> M.HashMap k0 (M.HashMap k1 a)
cleanup = M.filter (not . M.null)

union :: (Key a,Key b) 
      => Relation a b -> Relation a b -> Relation a b
union (Rel r0) (Rel r1) = Rel $ M.unionWith M.union r0 r1

intersection :: (Key a,Key b) 
             => Relation a b -> Relation a b -> Relation a b
intersection (Rel r0) (Rel r1) = Rel $ cleanup $ M.intersectionWith M.intersection r0 r1

difference :: (Key a,Key b) 
           => Relation a b -> Relation a b -> Relation a b
difference (Rel r0) (Rel r1) = Rel $ M.differenceWith f r0 r1
    where
        f x y
                | M.null z  = Nothing
                | otherwise = Just z
            where
                z = x `M.difference` y

subset :: (Key a,Key b) 
       => Relation a b -> Relation a b -> Bool
subset (Rel r0) (Rel r1) = M.isSubmapOfBy M.isSubmapOf r0 r1

irreflexive :: (Key a) => Relation a a -> Bool
irreflexive (Rel m) = M.null $ M.filterWithKey M.member m

transpose :: (Key a,Key b) 
          => Relation a b -> Relation b a
transpose (Rel m) = Rel $ M.map M.fromList $ M.unionsWith (++) $ M.elems $ M.mapWithKey (\k x -> M.map (const [(k,())]) x) m

symmetric :: (Key a) => Relation a a -> Bool
symmetric r = r == transpose r

images :: (Key a,Key b) 
       => Relation a b -> a -> S.HashSet b
images (Rel m) x = M.keysSet $ M.findWithDefault (M.empty) x m

transitive :: (Key a) => Relation a a -> Bool
transitive r = compose r r `subset` r

antisymmetric :: (Key a) => Relation a a -> Bool
antisymmetric r = null $ r `intersection` transpose r

null :: Relation a b -> Bool
null (Rel m) = M.null m

domain :: (Key a)
       => Relation a b -> S.HashSet a
domain (Rel m) = M.keysSet m

range :: (Key a,Key b) 
      => Relation a b -> S.HashSet b
range (Rel m) = M.keysSet $ M.unions $ M.elems m

(!) :: (Key a,Key b) 
    => Relation a b -> (a,b) -> Bool
(!) (Rel r) (x,y) = maybe False (y `M.member`) (x `M.lookup` r)

identity :: (Key a) => S.HashSet a -> Relation a a
identity s = Rel $ M.fromSet (`M.singleton` ()) s

(<|)  :: (Key a) 
      => S.HashSet a -> Relation a b -> Relation a b
(<<|) :: (Key a) 
      => S.HashSet a -> Relation a b -> Relation a b
(|>)  :: (Key b) 
      => Relation a b -> S.HashSet b -> Relation a b
(|>>) :: (Key b) 
      => Relation a b -> S.HashSet b -> Relation a b

(<|) = domRestr
(<<|) = domSubt
(|>) = ranRestr
(|>>) = ranSubt

domRestr :: (Key a) => S.HashSet a -> Relation a b -> Relation a b
domRestr s (Rel r) = Rel $ r `M.intersection` M.fromSet (const ()) s

domSubt :: (Key a) => S.HashSet a -> Relation a b -> Relation a b
domSubt s (Rel r) = Rel $ r `M.difference` M.fromSet (const ()) s

ranRestr :: (Key b) => Relation a b -> S.HashSet b -> Relation a b
ranRestr (Rel r) s = Rel $ M.mapMaybe f r
    where
        f m
                | M.null m' = Nothing
                | otherwise = Just m'
            where
                m' = m `M.intersection` M.fromSet (const ()) s

ranSubt :: (Key b) => Relation a b -> S.HashSet b -> Relation a b
ranSubt (Rel r) s = Rel $ M.mapMaybe f r
    where
        f m
                | M.null m' = Nothing
                | otherwise = Just m'
            where
                m' = m `M.difference` M.fromSet (const ()) s


mapDomain :: (Key a,Key b,Key c) => (a -> b) -> Relation a c -> Relation b c
mapDomain f (Rel m) = Rel $ M.mapKeysWith M.union f m

mapRange :: (Key b,Key c) => (b -> c) -> Relation a b -> Relation a c
mapRange f (Rel m) = Rel $ M.map (M.mapKeys f) m

bimap :: (Key a,Key b,Key c,Key d) => (a -> c) -> (b -> d) -> Relation a b -> Relation c d
bimap f g r = mapDomain f $ mapRange g r

filterDom :: (a -> Bool) -> Relation a b -> Relation a b
filterDom p (Rel m) = Rel $ M.filterWithKey (const . p) m

filterRan :: (b -> Bool) -> Relation a b -> Relation a b
filterRan p (Rel m) = Rel $ cleanup $ M.map (M.filterWithKey (const . p)) m

bimapMaybe :: (Key a0,Key a1,Key b0,Key b1)
           => (a0 -> Maybe a1)
           -> (b0 -> Maybe b1)
           -> Relation a0 b0
           -> Relation a1 b1
bimapMaybe f g = mapMaybeRan g . mapMaybeDom f

mapMaybeDom :: (Key a,Key b,Key c) => (a -> Maybe c) -> Relation a b -> Relation c b
mapMaybeDom p (Rel m) = Rel $ M.fromListWith M.union $ mapMaybe p' $ M.toList m
    where
        p' (x,y) = p x >>= \x -> return (x,y)

mapMaybeRan :: (Key b,Key c) => (b -> Maybe c) -> Relation a b -> Relation a c
mapMaybeRan p (Rel m) = Rel $ cleanup $ M.map (M.fromList . mapMaybe p' . M.toList) m
    where
        p' (x,y) = p x >>= \x -> return (x,y)

-- #if !(MIN_VERSION_QuickCheck(2,8,2))
-- instance (Key a,Arbitrary a) => Arbitrary (S.HashSet a) where
--     arbitrary = S.fromList `liftM` arbitrary
--     shrink = fmap S.fromList . genericShrink . S.toList
-- #endif
instance (Key a,Key b,Arbitrary a,Arbitrary b) => Arbitrary (Relation a b) where
    arbitrary = fromList `liftM` arbitrary
    shrink = fmap fromList . genericShrink . toList

imp_invariant :: Relation a b -> Bool
imp_invariant (Rel r) = M.null (M.filter M.null r)

prop_toList_fromList :: (Key b, Ord b, Key a, Ord a) 
                     => [(a, b)] -> Bool
prop_toList_fromList xs = sort (toList $ fromList xs) == LO.nubSort xs

prop_fromList_toList :: (Key b, Key a) 
                     => Relation a b -> Bool
prop_fromList_toList r = fromList (toList r) == r

prop_domain_def :: (Key a) => Relation a b -> Bool
prop_domain_def r = domain r == S.fromList (map fst $ toList r)

prop_range_def :: (Key a,Key b) => Relation a b -> Bool
prop_range_def r = range r == S.fromList (map snd $ toList r)

prop_empty_def :: (Arbitrary b, Arbitrary a, Show b, Show a, Key b, Key a) 
               => a -> b -> Bool
prop_empty_def x y = not $ empty ! (x,y)

prop_apply_def :: (Arbitrary t1, Arbitrary t, Show t1, Show t, Key t1, Key t) 
               => Relation t t1 -> Property
prop_apply_def r = forAll arbitrary $ \x y -> r ! (x,y) == ((x,y) `elem` toList r)

prop_compose_def :: (Ord a, Key a, Key b, Ord c, Key c) 
                 => Relation a b -> Relation b c -> Bool
prop_compose_def r0 r1 =   toList (compose r0 r1) 
                        == LO.nubSort [ (x,z) | (x,y) <- toList r0
                                           , (w,z) <- toList r1
                                           , y == w ]

closure :: (Ord a,Key a) => Relation a a -> Relation a a
closure r = Rel 
        $ cleanup
        $ M.map (M.fromSet (const ()) . S.fromList)
        $ Perm.closure' $ asGraph r

asGraph :: (Ord a,Key a) => Relation a a -> Perm.GraphImp a
asGraph r@(Rel m) = Perm.from_map (S.toList $ domain r `S.union` range r) 
        (M.map M.keys m)

cycles :: (Ord a,Key a) => Relation a a -> [[a]]
cycles r = Perm.cycles $ asGraph r

prop_all_valid :: (Ord b,Key b) => S.HashSet b -> Fun b b -> Fun b Bool -> Relation b b -> Relation b b -> Bool
prop_all_valid s f p r0 r1 = all imp_invariant 
            [ r0,r1
            , compose r0 r1
            , empty
            , union r0 r1
            , intersection r0 r1
            , difference r0 r1
            , identity s
            , s `domSubt` r0
            , s `domRestr` r0
            , r0 `ranSubt` s
            , r0 `ranRestr` s
            , transpose r0
            , mapDomain (apply f) r0
            , mapRange (apply f) r0
            , closure r0
            , filterRan (apply p) r0
            , filterDom (apply p) r0 ]

prop_union_def :: (Ord b,Key b,Ord a,Key a) 
               => Relation a b -> Relation a b -> Bool
prop_union_def r0 r1 = LO.nubSort (toList r0 ++ toList r1) == toList (r0 `union` r1)

prop_intersection_def :: (Ord b,Key b,Ord a,Key a) 
                      => Relation a b -> Relation a b -> Bool
prop_intersection_def r0 r1 = LO.nubSort (toList r0 `intersect` toList r1) == toList (r0 `intersection` r1)

prop_difference_def :: (Ord b,Key b,Ord a,Key a) => Relation a b -> Relation a b -> Bool
prop_difference_def r0 r1 = LO.nubSort (toList r0 \\ toList r1) == toList (r0 `difference` r1)

prop_identity_def :: (Key b) => S.HashSet b -> Bool
prop_identity_def s = toList (identity s) == map (\x -> (x,x)) (S.toList s)

prop_ranSubt :: (Eq a, Key b) => Relation a b -> S.HashSet b -> Bool
prop_ranSubt r s = toList (r `ranSubt` s) == filter (\(_,x) -> not $ x `S.member` s) (toList r)

prop_ranRestr :: (Key a, Key b) => Relation a b -> S.HashSet b -> Bool
prop_ranRestr r s = toList (r `ranRestr` s) == filter (\(_,x) -> x `S.member` s) (toList r)

prop_domSubt :: (Eq b, Key a) => Relation a b -> S.HashSet a -> Bool
prop_domSubt r s = toList (s `domSubt` r) == filter (\(x,_) -> not $ x `S.member` s) (toList r)

prop_domRestr :: (Eq b, Key a) => Relation a b -> S.HashSet a -> Bool
prop_domRestr r s = toList (s `domRestr` r) == filter (\(x,_) -> x `S.member` s) (toList r)

prop_transpose_def :: (Ord a,Key a,Ord b,Key b) => Relation b a -> Bool
prop_transpose_def r = toList (transpose r) == sort (map swap $ toList r)

prop_subset_def :: (Ord b,Key b,Ord a,Key a) => Relation a b -> Relation a b -> Bool
prop_subset_def r0 r1 = r0 `subset` r1 == toList r0 `LO.subset` toList r1

prop_irreflexive_def :: (Arbitrary b, Show b, Key b) => Relation b b -> Bool
prop_irreflexive_def r = (irreflexive r) == all (\x -> not $ r ! (x,x)) (S.toList $ domain r `S.union` range r)

prop_symmetric_def :: (Key b) => Relation b b -> Bool
prop_symmetric_def r = all (\x -> r ! swap x) (toList r) == symmetric r

prop_transitive_def :: (Key b) => Relation b b -> Bool
prop_transitive_def r = compose r r `subset` r == transitive r

prop_antisymmetric_def :: (Key b) => Relation b b -> Bool
prop_antisymmetric_def r = all (\x -> not $ r ! swap x) (toList r) == antisymmetric r

prop_null_def :: (Eq b, Eq a) => Relation a b -> Bool
prop_null_def r = null r == (r == empty)

prop_image_def :: (Key a, Key b) => Relation a b -> a -> Bool
prop_image_def r x = images r x == range (S.singleton x `domRestr` r)

prop_closure_is_transitive :: (Ord b,Key b) => Relation b b -> Bool
prop_closure_is_transitive r = (cl_r `compose` r) `union` r == cl_r
    where
        cl_r = closure r

prop_mapDomain_def :: (Key a,Ord b,Key b,Ord c,Key c) => Fun a b -> Relation a c -> Bool
prop_mapDomain_def (Fun _ f) r = toList (mapDomain f r) == LO.nubSort (map (first f) (toList r))

prop_mapRange_def :: (Ord b,Key b,Ord a,Key a,Key c) => Fun c b -> Relation a c -> Bool
prop_mapRange_def (Fun _ f) r = toList (mapRange f r) == LO.nubSort (map (second f) (toList r))

prop_cycles_all_valid :: (Ord b,Key b) => Relation b b -> Bool
prop_cycles_all_valid r = all (\c -> and [ cl ! (u,v) | u <- c, v <- c ] ) (cycles r)
    where
        cl = closure r

prop_cycles_maximal :: (Ord b,Key b) => Relation b b -> Bool
prop_cycles_maximal r = and $ do
        let r_cycle = cycles r
        (c0,cs) <- zip r_cycle $ drop 1 $ tails r_cycle
        c1 <- cs
        u <- c0
        v <- c1
        return $ not (cl ! (u,v)) || not (cl ! (v,u))
    where
        cl = closure r

prop_filterDom_def :: (Eq a, Eq b) => Fun a Bool -> Relation a b -> Bool
prop_filterDom_def (Fun _ p) r = toList (filterDom p r) == filter (p . fst) (toList r)

prop_filterRan_def :: (Eq a, Eq b) => Fun b Bool -> Relation a b -> Bool
prop_filterRan_def (Fun _ p) r = toList (filterRan p r) == filter (p . snd) (toList r)

prop_mapMaybeDom_def :: (Ord c,Key c,Ord b,Key b,Key b1) => Fun b1 (Maybe c) -> Relation b1 b -> Bool
prop_mapMaybeDom_def (Fun _ f) r = toList (mapMaybeDom f r) == LO.nubSort (mapMaybe (first' f) (toList r))
    where
        first' = runKleisli . first . Kleisli

prop_mapMaybeRan_def :: (Key b,Ord c,Key c,Ord a,Key a) => Fun b (Maybe c) -> Relation a b -> Bool
prop_mapMaybeRan_def (Fun _ f) r = toList (mapMaybeRan f r) == LO.nubSort (mapMaybe (second' f) (toList r))
    where
        second' = runKleisli . second . Kleisli

prop_bimapMaybe_def :: (Key a0,Ord a1,Key a1,Key b0,Ord b1,Key b1) 
                    => Fun a0 (Maybe a1) 
                    -> Fun b0 (Maybe b1) 
                    -> Relation a0 b0 -> Bool
prop_bimapMaybe_def (Fun _ f) (Fun _ g) r = 
        toList (bimapMaybe f g r) == LO.nubSort (mapMaybe (runKleisli $ f' *** g') (toList r))
    where
        f' = Kleisli f
        g' = Kleisli g

return []

run_spec :: (PropName -> Property -> IO (a, Result))
         -> IO ([a], Bool)
run_spec = $forAllProperties'

instance (NFData a,NFData b) => NFData (Relation a b)
