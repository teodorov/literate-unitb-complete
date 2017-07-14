{-# LANGUAGE TypeFamilies,TemplateHaskell #-}
module Logic.Expr.Scope 
    ( HasPrefix(..) 
    , module Logic.Expr.Scope )
where

    -- Modules
import Logic.Expr.Classes
import Logic.Expr.PrettyPrint
import Logic.Expr.Variable
import Logic.Names

    -- Libraries
import Control.Invariant
import Control.Lens
import Control.Monad.Reader.Class

import Data.Default
import Data.List
import Data.HashMap.Lazy as M
import Data.HashMap.Lazy.Extras as M

import GHC.Generics.Instances
import GHC.Stack.Utils

import PseudoMacros

import Text.Printf.TH

data VisibleVars = VisibleVars
        { _visibleVarsConstants :: HashMap Name Var
        , _visibleVarsAbs_vars  :: HashMap Name Var
        , _vars      :: HashMap Name Var
        , _prefix    :: [String]
        } deriving (Generic,Show)

type ScopeCorrectness  = ScopeCorrectnessM [(String,String)]
type ScopeCorrectnessM = ((->) VisibleVars)

class HasScope a where
    scopeCorrect' :: Pre => a -> ScopeCorrectness

makeLenses ''VisibleVars
makeFields ''VisibleVars

instance Default VisibleVars where
    def = genericDefault

scopeCorrect'' :: (HasScope a, PrettyPrintable lbl, Pre) 
               => lbl -> a -> ScopeCorrectness
scopeCorrect'' lbl x = withPrefix (pretty lbl) $ scopeCorrect' x

instance HasPrefix ScopeCorrectnessM where
    withPrefix lbl = local (prefix %~ (lbl:))

withVars :: (Foldable f) 
         => f Var 
         -> ScopeCorrectnessM a
         -> ScopeCorrectnessM a
withVars = withVars' constants

withVars' :: (Foldable f) 
          => Lens' VisibleVars (HashMap Name Var) -> f Var 
          -> ScopeCorrectnessM a
          -> ScopeCorrectnessM a
withVars' ln vs = local (ln %~ M.union (symbol_table vs))

withAbstract :: ScopeCorrectnessM a -> ScopeCorrectnessM a
withAbstract = local (\x -> x & vars %~ M.union (x^.abs_vars))

withoutConcrete :: ScopeCorrectnessM a -> ScopeCorrectnessM a
withoutConcrete = local $ vars .~ M.empty

withOnly :: Foldable f => f Var -> ScopeCorrectnessM a -> ScopeCorrectnessM a
withOnly vs = local (\x -> def & prefix .~ x^.prefix) . withVars vs

onlyAbstract :: ScopeCorrectnessM a -> ScopeCorrectnessM a
onlyAbstract = withoutConcrete . withAbstract

withPrimes :: ScopeCorrectnessM a -> ScopeCorrectnessM a
withPrimes cmd = do
    x <- ask
    withVars' vars (primeAll $ x^.vars) $ withVars' abs_vars (primeAll $ x^.abs_vars) cmd

areVisible :: (PrettyPrintable e,Foldable f,Pre) 
           => [Getting (HashMap Name Var) VisibleVars (HashMap Name Var)]
           -> f Var -> e -> ScopeCorrectness
areVisible ln vars' e = do
    vs <- foldMap view ln 
    let pref  = [s|\n%s\n free vars = %s\n declared  = %s\n diff      = %s|]
                (stackTrace' [$__FILE__] ?loc "Scope error")
                (show $ Pretty <$> M.keys vars)
                (show $ Pretty <$> M.keys vs)
                (show $ Pretty <$> M.keys (vars `M.difference` vs))
        vars = symbol_table vars'
    if vars `isSubmapOf` vs
        then return []
        else withPrefix pref $ do 
            pref <- views prefix $ intercalate " - " . reverse
            return [(pref,pretty e)]

scopeCorrect :: (HasScope a,Pre) => a -> [(String,String)]
scopeCorrect x = scopeCorrect' x def
