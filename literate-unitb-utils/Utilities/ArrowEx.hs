{-# LANGUAGE ConstraintKinds #-}
module Utilities.ArrowEx where

import Control.Arrow
import Control.Category
import Control.Lens
import Data.Functor.Compose
-- import Data.Functor.Const
-- import Data.Functor.Identity
import Data.Existential
import Data.Proxy

import Prelude hiding ((.),id)

class Arrow arr => ArrowEx arr where
    existsA1 :: (forall a. arr (Inst1 f cl a) (Inst1 g cl a))
             -> arr (Cell1 f cl) (Cell1 g cl)

returnConst :: Arrow arr
            => Proxy cl
            -> (forall a. arr (Inst1 f cl a) b)
            -> (forall a. arr (Inst1 f cl a) (Inst1 (Const b) cl a))
returnConst _ f = (f &&& id) >>> arr (\(x,Inst _) -> Inst $ Const x)

peelConst :: Arrow arr => arr a (Cell1 (Const b) cl) -> arr a b
peelConst x = x >>> arr (readCell1 getConst)

withClassProxy :: (Proxy cl -> arr (Cell1 f cl) b)
               -> arr (Cell1 f cl) b
withClassProxy f = f Proxy

_WrappedWithA :: Arrow arr
              => Iso s t a b
              -> arr t s
              -> arr b a
_WrappedWithA ln f = arr (getConst . from ln Const) >>> f >>> arr (getConst . ln Const)

_WrappedA :: (Rewrapped a b,Rewrapped b a,Arrow arr)
          => arr (Unwrapped a) (Unwrapped b)
          -> arr a b
_WrappedA = _WrappedWithA _Unwrapped

_UnwrappedWithA :: Arrow arr
                => Iso s t a b
                -> arr a b
                -> arr s t
_UnwrappedWithA ln f = arr (getConst . ln Const) >>> f >>> arr (getConst . from ln Const)

_UnwrappedA :: (Rewrapped a b,Rewrapped b a,Arrow arr)
            => arr a b
            -> arr (Unwrapped a) (Unwrapped b)
_UnwrappedA = _UnwrappedWithA _Unwrapped

existsA1' :: ArrowEx arr
          => (forall a. arr (Inst1 f cl a) b)
          -> arr (Cell1 f cl) b
existsA1' f = peelConst $ withClassProxy (\p -> existsA1 (returnConst p f))

wrapCellPair :: Arrow arr => arr (a,Cell1 f cl) (Cell1 (Compose ((,) a) f) cl)
wrapCellPair = arr (\(x,Cell y) -> Cell $ Compose (x,y))

unwrapCellPair :: Arrow arr => arr (Cell1 (Compose ((,) a) f) cl) (a,Cell1 f cl) 
unwrapCellPair = arr (\(Cell (Compose (x,y))) -> (x,Cell y))

cellPair :: Iso (k,Cell1 f cl) (k',Cell1 f' cl') 
                (Cell1 (Compose ((,) k) f) cl)
                (Cell1 (Compose ((,) k') f') cl')
cellPair = iso (\(x,Cell y) -> Cell $ Compose (x,y)) 
               (\(Cell (Compose (x,y))) -> (x,Cell y))

instPair :: Iso (k,Inst1 f cl a) (k',Inst1 f' cl' a') 
                (Inst1 (Compose ((,) k) f) cl a)
                (Inst1 (Compose ((,) k') f') cl' a')
instPair = iso (\(x,Inst y) -> Inst $ Compose (x,y)) 
               (\(Inst (Compose (x,y))) -> (x,Inst y))

existsA1Pair :: (ArrowEx arr)
             => (forall a. arr (k,Inst1 f cl a) (k', Inst1 g cl a))
             -> arr (k,Cell1 f cl) (k',Cell1 g cl)
existsA1Pair f = _UnwrappedWithA cellPair $ existsA1 (_WrappedWithA instPair f)

viewA :: Arrow arr => Getting a s a -> arr s a
viewA ln = arr (view ln)

existsA1Pair' :: (ArrowEx arr)
              => (forall a. arr (k,Inst1 f cl a) b)
              -> arr (k,Cell1 f cl) b
existsA1Pair' f = wrapCellPair >>> existsA1' (viewA (from instPair) >>> f)

existsA :: ArrowEx arr
        => (forall a. arr (Inst cl a) (Inst cl a))
        -> arr (Cell cl) (Cell cl)
existsA f = existsA1 f

existsA' :: ArrowEx arr
         => (forall a. arr (Inst cl a) b)
         -> arr (Cell cl) b
existsA' f = existsA1' f

instance ArrowEx (->) where
    existsA1 f = runIdentity . asInst (Identity . f) 
instance Monad m => ArrowEx (Kleisli m) where
    existsA1 f = Kleisli $ asInst $ runKleisli f
