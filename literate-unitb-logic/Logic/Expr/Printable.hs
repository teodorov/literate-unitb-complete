{-# LANGUAGE TypeFamilies #-}
module Logic.Expr.Printable where

import Logic.Expr.Expr
import Logic.Expr.PrettyPrint
import Logic.Expr.Scope

    -- Libraries
import Control.DeepSeq

import Data.Text (Text)
import Data.Typeable

import GHC.Generics
import GHC.Generics.Instances

import Language.Haskell.TH.Syntax

import Test.QuickCheck
import Test.QuickCheck.ZoomEq

instance HasExpr DispExpr where

instance HasGenExpr DispExpr where
    type ExprT DispExpr  = Expr
    asExpr (DispExpr _ e) = e
    ztrue   = DispExpr "\\true" ztrue
    zfalse  = DispExpr "\\false" zfalse
    zword v = DispExpr (prettyText v) (Word v)

data DispExpr = DispExpr !Text !Expr
    deriving (Show,Generic,Typeable)

instance Eq DispExpr where
    DispExpr _ x == DispExpr _ y = x == y

instance Ord DispExpr where
    DispExpr _ x `compare` DispExpr _ y = x `compare` y

instance HasScope DispExpr where
    scopeCorrect' = scopeCorrect' . getExpr

instance ZoomEq DispExpr where

instance Arbitrary DispExpr where
    arbitrary = do
        x <- arbitrary
        return $ DispExpr (prettyText x) x
    shrink = genericShrink

instance PrettyPrintable DispExpr where
    prettyText = prettyText . getExpr
    pretty = pretty . getExpr

prettyPrint :: DispExpr -> Text
prettyPrint (DispExpr x _) = x

instance NFData DispExpr

class FromDispExpr expr where
    fromDispExpr :: DispExpr -> expr

instance FromDispExpr Expr where
    fromDispExpr = getExpr

instance FromDispExpr DispExpr where
    fromDispExpr = id

instance Lift DispExpr where
    lift = genericLift
