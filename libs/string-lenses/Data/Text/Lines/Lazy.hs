{-# LANGUAGE TemplateHaskell,RankNTypes,CPP #-}
module Data.Text.Lines.Lazy where

import Control.Lens hiding (elements,(<|))

import Data.Foldable as F (fold)
import Data.List.NonEmpty (NonEmpty(..))
import           Data.Text.Lazy (Text)
import           Data.Text.Lens
import qualified Data.Text.Lazy as T
import qualified Data.String.Lines as LN

import Prelude hiding (lines,unlines)

asLines :: Iso' T.Text (NonEmpty T.Text)
asLines = iso lines' fold

traverseLines :: Traversal' T.Text T.Text
traverseLines = asLines . traverse

lines' :: Text -> NonEmpty Text
lines' = unpacked LN.lines'

