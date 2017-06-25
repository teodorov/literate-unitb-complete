{-# LANGUAGE OverloadedStrings #-}
module Logic.Expr.Label where

    -- Module
import Logic.Names

    -- Libraries
import Control.DeepSeq
import qualified Control.Invariant as I

import Data.Hashable
import Data.List as L
import Data.Serialize hiding (label)
import Data.Semigroup
import Data.String
import Data.Text as T (Text)
import qualified Data.Text as T
import Data.Typeable

import GHC.Generics 

import Test.QuickCheck hiding (label)
import Test.QuickCheck.ZoomEq

import Text.Pretty

data Label = Lbl Text
    deriving (Ord, Eq, Show, Typeable, Generic)

class IsLabel a where
    as_label :: a -> Label

instance PrettyPrintable Label where
    prettyText (Lbl s) = s

instance IsString Label where
    fromString x = label $ fromString x

instance Monoid Label where
    mempty  = Lbl ""
    mconcat = composite_label
    mappend = (</>)
instance Semigroup Label where

instance IsLabel Label where
    as_label = id

instance IsLabel Name where
    as_label = label . renderText . asInternal

instance (IsLabel l,IsLabel r) => IsLabel (Either l r) where
    as_label = either as_label as_label

instance ZoomEq Label where
    (.==) = (I.===)
instance Arbitrary Label where
    arbitrary = Lbl <$> elements [ fromString [x,y] | x <- ['a'..'z'], y <- ['0'..'9'] ]

instance Hashable Label where

label :: Text -> Label
label s = Lbl s

(</>) :: Label -> Label -> Label
(</>) (Lbl x) (Lbl y) 
        | T.null x || T.null y  = Lbl $ x <> y
        | otherwise         = Lbl $ x <> "/" <> y

composite_label :: [Label] -> Label
composite_label xs = Lbl $ T.intercalate "/" $ L.filter (not . T.null) $ map str xs
    where
        str (Lbl s) = s

to_list :: Label -> [Label]
to_list (Lbl xs) = map Lbl $ T.splitOn "/" xs

instance NFData Label

instance Serialize Label where
