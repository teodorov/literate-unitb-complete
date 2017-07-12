{-# LANGUAGE TemplateHaskell,RankNTypes,CPP #-}
module Data.Text.Lines where

import Control.Lens hiding (elements,(<|))

import Data.Foldable as F (fold)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (mapMaybe)
import           Data.Text (Text)
import           Data.Text.Lens
import qualified Data.Text as T
import qualified Data.String.Lines as Ln

import Prelude hiding (lines,unlines)

asLines :: Iso' T.Text (NonEmpty T.Text)
asLines = iso lines' fold

traverseLines :: Traversal' T.Text T.Text
traverseLines = asLines . traverse

lines' :: Text -> NonEmpty Text
lines' = unpacked Ln.lines'


