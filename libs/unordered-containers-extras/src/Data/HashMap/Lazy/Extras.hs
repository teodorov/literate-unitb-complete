{-# LANGUAGE ConstraintKinds, CPP, TupleSections #-}
module Data.HashMap.Lazy.Extras where

import Control.Applicative
import Control.Lens
import Control.Invariant
import Control.Monad
import Data.Default
import Data.Either.Combinators
import Data.Function
import Data.Hashable
import Data.HashMap.Lazy as M
-- import Data.HashMap.Strict as M (unsafeInsertWith)
import Data.HashSet as S
import Data.List as L
import Data.List.Ordered as L
import Data.Maybe (isJust,fromJust)
import GHC.Exts as L

type Key a = (Eq a,Hashable a)

notMember :: Key k => k -> HashMap k a -> Bool
notMember k = not . M.member k

elemAt :: Int -> HashMap k a -> (k, a)
elemAt i m = M.toList m !! i

asList :: (IsList l,IsList l') 
       => Iso l l' [Item l] [Item l']
asList = iso L.toList L.fromList

adjustWithKey :: Key k => (k -> a -> a) -> k -> HashMap k a -> HashMap k a
adjustWithKey f k = adjust (f k) k

mergeWithKey :: (Key k) 
             => (k -> a -> b -> Maybe c) 
             -> (HashMap k a -> HashMap k c) 
             -> (HashMap k b -> HashMap k c) 
             -> HashMap k a -> HashMap k b -> HashMap k c
mergeWithKey f fLeft fRight rs ls = M.unions [ fLeft ls', fRight rs', ms' ]
    where
        ls' = M.difference rs ls
        rs' = M.difference ls rs
        ms' = mapMaybe id $ intersectionWithKey f rs ls

mapKeys :: (Eq k1,Eq k2,Hashable k1,Hashable k2)
        => (k1 -> k2) -> HashMap k1 a -> HashMap k2 a
mapKeys f = asList.traverse._1 %~ f

mapKeysMonotonic :: (Eq k1,Eq k2,Hashable k1,Hashable k2)
                 => (k1 -> k2) -> HashMap k1 a -> HashMap k2 a
mapKeysMonotonic = mapKeys

mapKeysWith :: (Eq k1,Eq k2,Hashable k1,Hashable k2)
            => (a -> a -> a) 
            -> (k1 -> k2) 
            -> HashMap k1 a -> HashMap k2 a
mapKeysWith f g = iso M.toList (M.fromListWith f).traverse._1 %~ g

fromSet :: (Key k)
        => (k -> a) 
        -> HashSet k -> HashMap k a
fromSet f = M.mapWithKey (const . f) . toMap

keysSet :: (Key k)
        => HashMap k a -> HashSet k
keysSet = fromMap . (() <$)

findWithDefault :: (Key k) => a -> k -> HashMap k a -> a
findWithDefault x k m = lookupDefault x k m

isSubmapOf :: (Key k,Eq a)
           => HashMap k a 
           -> HashMap k a
           -> Bool
isSubmapOf = isSubmapOfBy (==)

isSubmapOf' :: (Key k,Eq a,Show k,Show a)
            => HashMap k a 
            -> HashMap k a
            -> Invariant
isSubmapOf' = relation "⊆" isSubmapOf

member' :: (Key k,Eq a,Show k,Show a)
        => k 
        -> HashMap k a
        -> Invariant
member' = relation "⊆" M.member

isSubmapOfBy :: (Key k)
             => (a -> b -> Bool) 
             -> HashMap k a 
             -> HashMap k b 
             -> Bool
isSubmapOfBy f m0 m1 = M.null $ differenceWith (\x y -> guard (not $ f x y) >> Just x) m0 m1

foldMapWithKey :: Monoid m => (k -> a -> m) -> HashMap k a -> m
foldMapWithKey f = foldrWithKey (\k -> mappend . f k) mempty

unionsWith :: (Key k) => (a -> a -> a) -> [HashMap k a] -> HashMap k a
unionsWith f = L.foldl' (M.unionWith f) M.empty

(\\) :: Key k => HashMap k a -> HashMap k b -> HashMap k a
m1 \\ m2 = M.difference m1 m2

partition :: (a -> Bool) -> HashMap k a -> (HashMap k a, HashMap k a)
partition p = liftA2 (,) (mapMaybe leftToMaybe) (mapMaybe rightToMaybe) . M.map (\x -> if p x then Left x else Right x)

partitionWithKey :: (k -> a -> Bool) -> HashMap k a -> (HashMap k a, HashMap k a)
partitionWithKey p = liftA2 (,) (mapMaybe leftToMaybe) (mapMaybe rightToMaybe) . M.mapWithKey (\k x -> if p k x then Left x else Right x)

mapEither :: (a -> Either b c) -> HashMap k a -> (HashMap k b, HashMap k c)
mapEither f = liftA2 (,) (mapMaybe leftToMaybe) (mapMaybe rightToMaybe) . M.map f

fromListWithKey :: (Eq k, Hashable k) 
                => (k -> v -> v -> v) 
                -> [(k, v)] -> HashMap k v
fromListWithKey f = fmap snd . fromListWith (\(k,x) (_,y) -> (k,f k x y)) . L.map withKey
    where
        withKey x = (fst x,x)

#if !MIN_VERSION_unordered_containers(0,2,6)
mapMaybe :: (v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
mapMaybe f = M.map fromJust . M.filter isJust . M.map f

-- #endif
-- #if !MIN_VERSION_unordered_containers(0,2,6)
differenceWith :: (Eq k, Hashable k) 
               => (v -> w -> Maybe v) 
               -> HashMap k v 
               -> HashMap k w 
               -> HashMap k v
differenceWith f m0 m1 = M.union md (M.map fromJust . M.filter isJust $ mi)
    where
        md = M.difference m0 m1
        mi = intersectionWith f m0 m1

intersectionWithKey :: (Eq k, Hashable k) 
                    => (k -> v0 -> v1 -> v2) 
                    -> HashMap k v0
                    -> HashMap k v1
                    -> HashMap k v2
intersectionWithKey f m0 m1 = M.intersectionWith (uncurry f) (M.mapWithKey (,) m0) m1

-- #endif
-- #if !MIN_VERSION_unordered_containers(0,2,6)
fromMap :: Key k => HashMap k () -> HashSet k 
fromMap = S.fromList . M.keys

toMap :: Key k => HashSet k -> HashMap k ()
toMap = M.fromList . L.map (,()) . S.toList
#endif

toAscList :: Ord k => HashMap k a -> [(k, a)]
toAscList = sortOn fst . M.toList

instance Default (HashMap k a) where
    def = M.empty

instance (Ord k,Ord a) => Ord (HashMap k a) where
    compare = compare `on` M.toList
