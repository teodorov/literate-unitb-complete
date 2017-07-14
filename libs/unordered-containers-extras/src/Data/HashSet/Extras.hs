module Data.HashSet.Extras where

import Data.HashMap.Lazy.Extras hiding ((\\))
import Data.HashSet as S

notMember :: (Key a)
          => a -> HashSet a -> Bool
notMember x m = not $ member x m

(\\) :: (Key a)
     => HashSet a 
     -> HashSet a 
     -> HashSet a
m0 \\ m1 = difference m0 m1

isSubsetOf :: (Key a)
           => HashSet a 
           -> HashSet a 
           -> Bool
isSubsetOf s0 s1 = S.null $ s0 \\ s1
