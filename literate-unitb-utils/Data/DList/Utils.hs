module Data.DList.Utils where

import Control.Lens

import Data.DList as D
import Data.List  as L

intercalate :: [a] -> [DList a] -> DList a
intercalate xs xss = D.concat (L.intersperse (D.fromList xs) xss)

intersperse :: a -> [DList a] -> DList a
intersperse xs xss = D.concat (L.intersperse (D.singleton xs) xss)

concatMap :: (a -> DList b) 
          -> [a]
          -> DList b
concatMap f = D.concat . L.map f

asList :: Iso (DList a) (DList b) [a] [b]
asList = iso D.toList D.fromList
