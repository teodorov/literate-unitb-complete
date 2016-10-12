module Control.Lens.Misc where

import Control.Lens
import Control.Monad.State

import Data.Default
import Data.Either.Validation
import Data.Foldable as F
import Data.Map as M
import Data.Semigroup


{-# INLINE withKey #-}
withKey :: Iso (Map a b) (Map c d) (Map a (a,b)) (Map c d)
withKey = iso (M.mapWithKey (,)) id

{-# INLINE withKey' #-}
withKey' :: Getter (Map a b) (Map a (a,b))
withKey' = to (M.mapWithKey (,))

create :: Default a => State a b -> a
create cmd = execState cmd def

combine :: Lens' b a -> (a -> a -> a) -> b -> b -> b -> b
combine ln f x y z = z & ln .~ f (x^.ln) (y^.ln)

combine' :: Lens' b a -> (a -> a -> a) -> b -> b -> State b ()
combine' ln f x y = modify $ combine ln f x y

combineAll :: (Foldable f, Functor f, Default a) 
           => Lens' b a -> (a -> a -> a) -> f b -> b -> b
combineAll ln f xs = set ln $ F.foldl' f def $ view ln <$> xs

combineAll' :: (Foldable f, Functor f, Default a) 
            => Lens' b a -> (a -> a -> a) -> f b -> State b ()
combineAll' ln f xs = modify $ combineAll ln f xs

onBoth :: Applicative f
       => (a0 -> f a1)
       -> (b0 -> f b1)
       -> (a0,b0)
       -> f (a1,b1)
onBoth f g (x,y) = (,) <$> f x <*> g y

traverseValidation :: (Traversable t,Semigroup e)
                   => (a -> Either e b) 
                   -> t a -> Either e (t b)
traverseValidation f = validationToEither . traverse (eitherToValidation . f)

itraverseValidation :: (TraversableWithIndex k t,Semigroup e)
                    => (k -> a -> Either e b) 
                    -> t a -> Either e (t b)
itraverseValidation f = validationToEither . itraverse (fmap eitherToValidation . f)

unzipped :: Iso [(a,b)] [(c,d)] ([a],[b]) ([c],[d])
unzipped = iso unzip (uncurry zip)

zipped :: Iso ([a],[b]) ([c],[d]) [(a,b)] [(c,d)]
zipped = from unzipped

asListWith :: Ord k'
           => (a' -> a' -> a')
           -> Iso (Map k a) (Map k' a')
                  [(k,a)] [(k',a')]
asListWith f = iso M.toList (M.fromListWith f)

uncurryMap :: (Ord a,Ord b)
           => Map a (Map b c)
           -> Map (a,b) c
uncurryMap m = fromList [ ((x,y),k) | (x,xs) <- M.toList m, (y,k) <- M.toList xs ]

curryMap :: (Ord a,Ord b)
         => Map (a,b) c
         -> Map a (Map b c)
curryMap m = fromList <$> fromListWith (++) [ (x,[(y,k)]) | ((x,y),k) <- M.toList m ]

curriedMap :: (Ord a,Ord b,Ord x,Ord y)
           => Iso (Map (a,b) c) (Map (x,y) z) 
                  (Map a (Map b c)) (Map x (Map y z))
curriedMap = iso curryMap uncurryMap
