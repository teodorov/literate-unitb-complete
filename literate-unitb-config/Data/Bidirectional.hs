module Data.Bidirectional where

import Control.Lens hiding (set)
import Control.Monad

import Data.ConfigFile
import Data.Either.Combinators
import Data.Map as M
import Data.Maybe

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

class Document a where
    makeNode :: String -> String -> a -> a
    lookupDoc :: String -> a -> Maybe String

instance Document ConfigParser where
    makeNode k x c = fromRight' $ set c "DEFAULT" k x
    lookupDoc x m = rightToMaybe $ get m "DEFAULT" x 

instance Document (Map String String) where
    makeNode = M.insert
    lookupDoc = M.lookup

field :: Document doc
      => String -> (b -> String) 
      -> BiParser Maybe b doc String
field k f = BiParser (makeNode k . f) (lookupDoc k)

fieldWith :: (Document doc,Applicative f)
          => String
          -> String 
          -> (c -> String)
          -> BiParser f c doc String
fieldWith def k f = BiParser 
            (\x doc -> 
                    let x' = f x in
                    if x' == fromMaybe def (lookupDoc k doc) 
                        then doc
                        else makeNode k x' doc ) 
            (pure.fromMaybe def.lookupDoc k)

field' :: (Document doc,Show a,Read a) 
       => String -> (b -> a) 
       -> BiParser Maybe b doc a
field' k f = BiParser (makeNode k . show . f) (readMaybe <=< lookupDoc k)

fieldWith' :: (Document doc,Show a,Read a,Applicative f,Eq a) 
           => a -> String -> (c -> a)
           -> BiParser f c doc a
fieldWith' def k f = BiParser 
            (\x doc -> 
                    let x' = f x in
                    if x' == fromMaybe def (readMaybe =<< lookupDoc k doc) 
                        then doc
                        else makeNode k (show x') doc ) 
            (pure.fromMaybe def.(readMaybe <=< lookupDoc k))
            -- (pure def)
    -- withDefault x . field' k

-- withDefault :: Applicative f 
--             => a 
--             -> BiParser Maybe c b a 
--             -> BiParser f c b a
-- withDefault x (BiParser f g) = BiParser f (pure . fromMaybe x . g)

optionally :: Applicative f
           => BiParser Maybe c b a 
           -> BiParser f c b (Maybe a)
optionally (BiParser f g) = BiParser f (pure . g)
