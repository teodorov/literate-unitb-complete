module Data.Bidirectional where

import Control.Lens hiding (set)
import Control.Monad

import Data.Either.Combinators
import Data.HashMap.Strict as H
import Data.Maybe
import Data.Text (pack)
import Data.Yaml (Object, ToJSON, FromJSON,
                  toJSON, parseJSON, parseMaybe)

import Text.Read hiding (get)

data BiParser f a b c = BiParser (a -> b -> b) (b -> f c)
    deriving (Functor)

instance (Applicative f) => Applicative (BiParser f a b) where
    pure x = BiParser (const id) (const $ pure x)
    BiParser f0 g0 <*> BiParser f1 g1 = BiParser 
        (\x -> f0 x . f1 x) 
        (\x -> g0 x <*> g1 x)

-- instance Profunctor (BiParser a) where
--     dimap f g (BiParser p q) = BiParser (_.p) (fmap g.q.f)

lensOf :: BiParser Identity a s a
       -> Lens' s a
lensOf (BiParser f g) = lens (runIdentity . g) (\x i -> f i x)

prismOf :: s
        -> BiParser Maybe a s a
        -> Prism' s a
prismOf def (BiParser f g) = prism' (\x -> f x def) g

traverseOf :: s
           -> BiParser Identity a s a
           -> Traversal' s a
traverseOf def (BiParser f g) h x =
    (\i -> f i def) <$> h (runIdentity $ g x)
    -- f <$> h (runIdentity $ g x) <*> pure def

class Document a where
    makeNode :: (ToJSON b) => String -> b -> a -> a
    lookupDoc :: (FromJSON b) => String -> a -> Maybe b

instance Document Object where
    makeNode k x = H.insert (pack k) (toJSON x)
    lookupDoc k m = do
        v <- H.lookup (pack k) m
        parseMaybe parseJSON v

field :: (Document doc,ToJSON a,FromJSON a)
       => String -> (b -> a)
       -> BiParser Maybe b doc a
field k f = BiParser (makeNode k . f) (lookupDoc k)

fieldWith :: (Document doc,Applicative f,Eq a,ToJSON a,FromJSON a)
           => String -> a -> (c -> a)
           -> BiParser f c doc a
fieldWith k def f = BiParser
            (\x doc -> 
                    let x' = f x in
                    if x' == fromMaybe def (lookupDoc k doc) 
                        then doc
                        else makeNode k x' doc ) 
            (pure.fromMaybe def.lookupDoc k)

optionally :: Applicative f
           => BiParser Maybe c b a 
           -> BiParser f c b (Maybe a)
optionally (BiParser f g) = BiParser f (pure . g)
