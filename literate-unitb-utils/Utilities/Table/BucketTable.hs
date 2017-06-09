{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Utilities.Map.BucketMap where

import Control.Arrow
import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Precondition ((!))

import Data.Default
import Data.Function
import Data.Hashable
import qualified Data.Map as IM
import qualified Data.Map as M
import qualified Data.Maybe as My
import qualified Data.List as L
import Data.Map.Class
import Data.Semigroup
import Data.Serialize hiding (Result)
import qualified Data.Set as S

import GHC.Generics.Instances

import Prelude hiding (lookup,null,map,filter)


import Test.QuickCheck hiding (shrinkList)
import Test.QuickCheck.Function
import Test.QuickCheck.Report

type Bucket = M.Map

newtype HashMap a b = HashMap { _hashMap :: M.Map Int (Bucket a b) }
    deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Default,Monoid)

newtype OrderedBucket a b = OrderedBucket { _orderedBucket :: [(a,b)] }
    deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Default,Monoid)

newtype UnorderedBucket a b = UnorderedBucket { _unorderedBucket :: [(a,b)] }
    deriving (Eq,Ord,Generic,Functor,Foldable,Traversable,Default,Monoid)

type instance Index (HashMap a b) = a
type instance IxValue (HashMap a b) = b
type instance Index (OrderedBucket a b) = a
type instance IxValue (OrderedBucket a b) = b
type instance Index (UnorderedBucket a b) = a
type instance IxValue (UnorderedBucket a b) = b

--makeLenses ''Map
makeLenses ''HashMap
makeLenses ''OrderedBucket
makeLenses ''UnorderedBucket

--{-# INLINE bucket #-}
--bucket :: Ord k1 => Iso (Bucket k0 a0) (Bucket k1 a1) [(k0,a0)] [(k1,a1)]
--bucket = iso M.toList M.fromList

--{-# INLINE unbucket #-}
--unbucket :: Ord k0 => Iso [(k0,a0)] [(k1,a1)] (Bucket k0 a0) (Bucket k1 a1)
--unbucket = from bucket

instance (Ord a,Hashable a) => At (HashMap a b) where
    at i = hashMap.at (hash i).notNull'.at i

instance (Ord a,Hashable a) => Ixed (HashMap a b) where
    ix i = at i.traverse

instance (Ord a) => At (OrderedBucket a b) where
    at i = orderedBucket.lens (L.lookup i) (\xs r -> insertMaybe i r $ L.filter ((i /=) . fst) xs)

insertMaybe :: Ord k => k -> Maybe a -> [(k,a)] -> [(k,a)]
insertMaybe k = maybe id (L.insertBy (compare `on` fst) . (k,))

instance (Ord a) => Ixed (OrderedBucket a b) where
    ix i = at i.traverse

instance (Ord a) => At (UnorderedBucket a b) where
    at i = unorderedBucket.lens (L.lookup i) (\xs r -> maybe id ((:) . (i,)) r $ L.filter ((i /=) . fst) xs)

instance (Ord a) => Ixed (UnorderedBucket a b) where
    ix i = at i.traverse

instance (Show a,Show b,Ord a) => Show (HashMap a b) where
    show m = "fromList " ++ show (toList m)

instance (NFData a,NFData b) => NFData (HashMap a b) where
instance (NFData a,NFData b) => NFData (OrderedBucket a b) where
instance (NFData a,NFData b) => NFData (UnorderedBucket a b) where

instance (Serialize a,Serialize b,Ord a) => Serialize (HashMap a b) where
instance (Serialize a,Serialize b,Ord a) => Serialize (OrderedBucket a b) where
instance (Serialize a,Serialize b,Ord a) => Serialize (UnorderedBucket a b) where

{-# SPECIALIZE notNull' :: Lens' (Maybe (Bucket k a)) (Bucket k a) #-}
{-# SPECIALIZE notNull' :: Lens' (Maybe (HashMap k a)) (HashMap k a) #-}
notNull' :: IsMap map => Lens' (Maybe (map k a)) (map k a)
notNull' = lens (My.fromMaybe empty) (const notNull)

{-# SPECIALIZE notNull :: Bucket a b -> Maybe (Bucket a b) #-}
{-# SPECIALIZE notNull :: HashMap a b -> Maybe (HashMap a b) #-}
notNull :: IsMap map => map a b -> Maybe (map a b)
notNull m | null m    = Nothing
          | otherwise = Just m

instance IsMap HashMap where
    type IsKey HashMap k = (Ord k,Hashable k)
    {-# INLINE null #-}
    null  = IM.null . view hashMap
    {-# INLINE empty #-}
    empty = HashMap IM.empty
    {-# INLINE singleton #-}
    singleton k x = HashMap $ IM.singleton (hash k) (singleton k x)
    -- {-# INLINE size #-}
    size = sum . IM.map size . view hashMap
    -- {-# INLINE isSubmapOf #-}
    isSubmapOf = isSubmapOfBy (==)
    -- {-# INLINE isSubmapOfBy #-}
    isSubmapOfBy f x y = IM.isSubmapOfBy (isSubmapOfBy f) (x^.hashMap) (y^.hashMap)
    -- {-# INLINE isProperSubmapOf #-}
    isProperSubmapOf x y = IM.isProperSubmapOfBy isProperSubmapOf (x^.hashMap) (y^.hashMap)
        -- Map
    -- {-# INLINE map #-}
    map f = hashMap %~ IM.map (map f)
    -- {-# INLINE mapMaybe #-}
    mapMaybe f = hashMap %~ IM.mapMaybe (notNull . mapMaybe f)
    mapMaybeWithKey f = hashMap %~ IM.mapMaybe (notNull . mapMaybeWithKey f)
    -- {-# INLINE mapEither #-}
    mapEither f = mapEitherWithKey (const f)
    -- {-# INLINE mapEitherWithKey #-}
    mapEitherWithKey f = (     HashMap . IM.mapMaybe (notNull . fst) 
                           &&& HashMap . IM.mapMaybe (notNull . snd)) 
                . IM.map (mapEitherWithKey f) . view hashMap
    -- {-# INLINE mapWithKey #-}
    mapWithKey f = hashMap %~ IM.map (mapWithKey f)
    -- {-# INLINE traverseWithKey #-}
    traverseWithKey f = hashMap $ traverse $ traverseWithKey f
    -- {-# INLINE foldMapWithKey #-}
    foldMapWithKey f = foldMap (foldMapWithKey f) . view hashMap
    -- {-# INLINE mapKeys #-}
    mapKeys f = asList.traverse._1 %~ f
    -- {-# INLINE mapKeysWith #-}
    mapKeysWith f g = asListWith f.traverse._1 %~ g
    -- {-# INLINE mapKeysMonotonic #-}
    mapKeysMonotonic = mapKeys
        -- change by one
    -- {-# INLINE insert #-}
    insert k x = hashMap %~ IM.insertWith union (hash k) (singleton k x)
    -- {-# INLINE delete #-}
    delete k = hashMap %~ IM.update (notNull . delete k) (hash k)
    -- {-# INLINE adjustWithKey #-}
    adjustWithKey f k = hashMap %~ IM.adjust (adjustWithKey f k) (hash k)
        -- lookup
    -- {-# INLINE elemAt #-}
    elemAt i = (!i) . toAscList
    -- {-# INLINE lookup #-}
    lookup k = lookup k <=< IM.lookup (hash k) . view hashMap
    member k = maybe False (member k) . IM.lookup (hash k) . view hashMap
    -- {-# INLINE findWithDefault #-}
    findWithDefault x k = My.fromMaybe x . lookup k
        -- filtering
    filter f = hashMap %~ IM.mapMaybe (notNull . filter f)
    filterWithKey f = hashMap %~ IM.mapMaybe (notNull . filterWithKey f)
    partition = partitionWithKey . const
    partitionWithKey f = mapEitherWithKey f'
        where f' k x | f k x     = Left x
                     | otherwise = Right x
    split k = partitionWithKey (\k' _ -> k' < k) . filterWithKey (const . (k /=))
        -- Combination
    union = unionWith const
    unionWith f = unionWithKey (const f)
    unionWithKey f (HashMap m0) (HashMap m1) = HashMap $ IM.unionWith (unionWithKey f) m0 m1
    unions = unionsWith const
    unionsWith f = HashMap . IM.unionsWith (unionWith f) . L.map (view hashMap)
    intersection = intersectionWith const
    intersectionWith f = intersectionWithKey (const f)
    intersectionWithKey f (HashMap m0) (HashMap m1) = HashMap $ IM.intersectionWith (intersectionWithKey f) m0 m1
    difference = differenceWith (\ _ _ -> Nothing)
    differenceWith f (HashMap m0) (HashMap m1) = 
            HashMap $ IM.differenceWith (fmap notNull <$> differenceWith f) m0 m1
        -- lists
    keys  = concat . IM.elems . IM.map keys . view hashMap
    keysSet  = S.unions . IM.elems . IM.map keysSet . view hashMap
    elems = ascElems
    ascElems = L.map snd . toAscList
    toList = toAscList
    --toList = concat . IM.elems . IM.map toList . view hashMap
    toAscList = toAscList . unions . IM.elems . view hashMap
    toListIntl = (Unordered,) . tableToList'
    fromSet f = fromList . L.map (id &&& f) . S.toList
    fromList = fromListWith const
    fromListIntl (_,xs) = fromList xs
    fromListWith f    = fromListWithKey (const f)
    fromListWithKey f xs = HashMap $ IM.map (fromListWithKey f) 
                                     $ IM.fromListWith (flip (++)) (L.map g xs)
                where g (k,x) = (hash k,[(k,x)])
    tableToList = tableToList'
    tableElems = tableElems'

prop_unionsWith_consistent :: forall k a. (Ord k,Eq a,Show k,Show a,Hashable k)
                           => Fun a (Fun a a) -> [[(k,a)]]
                           -> Property
prop_unionsWith_consistent f' xs = 
        M.toList (unionsWith f $ L.map fromList xs)
        === 
        toList (unionsWith f $ L.map fromList xs :: HashMap k a)
    where
        f = apply . apply f'

tableToList' :: HashMap k a -> [(k,a)]
tableToList' = concat . IM.elems . IM.map M.toList . view hashMap

tableElems' :: HashMap k a -> [a]
tableElems' = concat . IM.elems . IM.map M.elems . view hashMap

_foo :: HashMap k a -> [a]
_foo = tableElems'

instance (Hashable k,Ord k) => Semigroup (Intersection (HashMap k a)) where
    Intersection x <> Intersection y = Intersection $ x `intersection` y


return []

run_spec :: (PropName -> Property -> IO (a,Result)) 
         -> IO ([a],Bool)
run_spec = $forAllProperties'
