{-# LANGUAGE TypeFamilies #-}
module Logic.Expr.Printable where

import Logic.Expr.Expr
import Logic.Expr.Type
import Logic.Expr.Scope

    -- Libraries
import Control.DeepSeq

import Data.Typeable

import GHC.Generics

import Test.QuickCheck

instance IsExpr DispExpr where
    getExpr (DispExpr _ e) = e

instance IsGenExpr DispExpr where
    type AnnotT DispExpr = Type
    type TypeT DispExpr  = Type
    type QuantT DispExpr = HOQuantifier
    type ExprT DispExpr  = Expr
    asExpr = getExpr
    ztrue  = DispExpr "\\true" ztrue
    zfalse = DispExpr "\\false" zfalse

data DispExpr = DispExpr String Expr
    deriving (Generic,Typeable)

instance Eq DispExpr where
    DispExpr _ x == DispExpr _ y = x == y

instance Ord DispExpr where
    DispExpr _ x `compare` DispExpr _ y = x `compare` y

instance Show DispExpr where
    show (DispExpr _ x) = show x

instance HasScope DispExpr where
    scopeCorrect' = scopeCorrect' . getExpr

instance Arbitrary DispExpr where
    arbitrary = do
        x <- arbitrary
        return $ DispExpr (show x) x

prettyPrint :: DispExpr -> String
prettyPrint (DispExpr x _) = x

instance NFData DispExpr

