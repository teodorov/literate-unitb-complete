module Utilities.MapSyntax where

    -- Libraries
import Control.Monad.Writer

import Data.HashMap.Lazy as M

newtype MapSyntax k a b = MapSyntax (Writer [(k,a)] b)
    deriving (Functor,Applicative,Monad)

(##) :: k -> a -> MapSyntax k a ()
x ## y = MapSyntax (tell [(x,y)])

runMapWith :: (Ord k) 
           => (a -> a -> a) 
           -> MapSyntax k a b 
           -> HashMap k a
runMapWith f (MapSyntax cmd) = M.fromListWith f $ execWriter cmd

runMap' :: (Ord k) 
        => MapSyntax k a b 
        -> HashMap k a
runMap' = runMapWith const
