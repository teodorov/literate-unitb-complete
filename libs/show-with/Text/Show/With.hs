{-# LANGUAGE CPP #-}
module Text.Show.With where

#if MIN_VERSION_transformers(0,5,0)
import Prelude.Extras
#else
import Data.Functor.Classes
import GHC.Generics.Instances
#endif
import           Data.Text (Text,unpack)

newtype Verbatim a = Verbatim a
type VerbatimString = Verbatim String
type VerbatimSString = Verbatim (String -> String)
type VerbatimText = Verbatim Text

instance Show VerbatimText where
    showsPrec _ (Verbatim x) = (unpack x ++)
instance Show VerbatimString where
    showsPrec _ (Verbatim x) = (x ++)

instance Show VerbatimSString where
    showsPrec _ (Verbatim f) = f

showWith' :: (Functor f, Show1 f)
          => (a -> ShowS)
          -> f a -> String
showWith' f = ($ "") . showsWith f

showWith :: (Functor f, Show1 f)
         => (a -> String)
         -> f a -> String
showWith f = showWith' ((++) . f)

showsWith :: (Functor f, Show1 f)
          => (a -> ShowS)
          -> f a -> ShowS
showsWith f = shows1 . fmap (Verbatim . f)
