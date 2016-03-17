{-# LANGUAGE RecordWildCards #-}
module Theories.Arithmetic where

    -- Modules
import Logic.Expr
import Logic.Expr.Const
import Logic.Operator
import Logic.Proof hiding (syntacticProp)
import Logic.Theory
import Logic.Theory.Monad

import Theories.SetTheory
import Theories.FunctionTheory

    -- Libraries
import Control.Arrow
import Control.Lens

import Data.List as L
import Data.Map.Class

import Utilities.Lens

    -- arithmetic
power   :: BinOperator
mult    :: BinOperator
plus    :: BinOperator
minus   :: BinOperator
less    :: BinOperator
greater :: BinOperator
leq     :: BinOperator
geq     :: BinOperator

power   = make BinOperator "^" "^"         Direct  pow_fun
mult    = make BinOperator "*" "\\cdot"    Direct  times_fun
plus    = make BinOperator "+" "+"         Direct  plus_fun
minus   = make BinOperator "-" "-"         Direct  minus_fun
less    = make BinOperator "<" "<"         Direct  less_fun
greater = make BinOperator ">" ">"         Flipped less_fun
leq     = make BinOperator "<=" "\\le"     Direct  le_fun
geq     = make BinOperator ">=" "\\ge"     Flipped le_fun

zsum :: [Var] -> ExprP -> ExprP -> ExprP
zsum = zquantifier qsum

card_fun :: IsName n => AbsFun n Type
card_fun = Fun [gA] [smt|card|] Unlifted [set_type gA] int FiniteWD

zcard :: ExprP -> ExprP
zcard = typ_fun1 card_fun

gT :: Type
gT = VARIABLE $ fromString'' "t"

arithmetic :: Theory
arithmetic = (empty_theory' "arithmetic") { 
        _extends = symbol_table [set_theory]
        , _types = symbol_table [IntSort,RealSort]
        , _funs = symbol_table 
            [ sum_fun gA 
            , card_fun ]
        , _theorySyntacticThm = empty_monotonicity
            { _associative  = fromList [(fromString'' "+",mzint 0)] 
            , _monotonicity = fromList $ L.map (first $ z3Name *** z3Name)
              [ (("=>","<="), Side (Just zge' )
                                   (Just zle'))
              , (("<=","+"), Independent zle')
              , (("<=","-"),  Side (Just zge') 
                                   (Just zle')) ] }
        , _fact = "arithmetic" `axioms` do
                $axiom $ 
                    asum zempty_set term `mzeq` mzint 0

                $axiom $ 
                    (mznot $ x `zelem` r) .=>
                         asum (r `zunion` zmk_set x) term  
                    .=. (asum r term .+ zselect term x)

                $axiom $ 
                    (r `zintersect` r' .=. zempty_set) .=>
                         asum (r `zunion` r') term 
                    .=. (asum r term .+ asum r' term)

                $axiom $ mzfinite r .=>
                    mzint 0 .<= zcard r

                $axiom $ 
                    zcard r .=. mzint 0  .==.  r .=. zempty_set

                $axiom $ 
                    zcard (zmk_set x) .=. mzint 1

                $axiom $
                         zcard r .=. mzint 1
                    .==. mzexists [x_decl] mztrue (r .=. zmk_set x)

                    -- dangerous!
                -- $axiom $ 
                --     mznot (x `zelem` r) .=>
                --          zcard (r `zunion` zmk_set x)
                --     .=.  zcard r .+ mzint 1

                $axiom $ 
                    r `zintersect` r' .=. zempty_set .=>
                         zcard (r `zunion` r')
                    .=.  zcard r .+ zcard r'            
                $axiom $
                    zcard r .=. typ_fun2 (sum_fun gA) r (zconst $ mzint 1)

            -- fromList $ L.map (first $ label . dec')
            -- [ ("0",axm1) 
            -- , ("1",axm2)
            -- , ("2",axm3)
            -- , ("3",axm4)
            -- -- , ("4",axm5)
            -- , ("5",axm6) 
            -- , ("6",axm7)
            -- , ("7",axm8)
            -- -- , ("8",axm9)
            -- -- , ("9",axm10)
            -- ]
        , _notation = arith }
    where
        zle' = Rel le_fun Direct
        zge' = Rel le_fun Flipped
        -- cast = zcast (set_type gT)
        asum = typ_fun2 (sum_fun gA)
        (term,_term_decl) = var "term" (array gT int)
        (r,_r_decl) = var "r" (set_type gT)
        (r',_r'_decl) = var "r0" (set_type gT)
        (x,x_decl) = var "x" gT
        
sum_fun :: Type -> Fun
sum_fun t = mk_fun [t] [smt|qsum|] [set_type t, array t int] int

qsum :: HOQuantifier
qsum = UDQuant (sum_fun gA) int (QTConst int) FiniteWD

arith :: Notation
arith = create $ do
   new_ops     .= L.map Right [power,mult,plus,leq,geq
                                ,less,greater,minus]
   prec .= [ L.map (L.map Right)
                     [ [apply]
                     , [power]
                     , [mult]
                     , [plus,minus]
                     , [mk_fun_op]
                     , [ equal,leq
                       , less
                       , geq,greater]]]
   left_assoc  .= [[mult],[plus]]
   right_assoc .= []
   relations   .= [equal,leq,geq,less,greater]
   chaining    .= 
          [ ((leq,leq),leq)
          , ((leq,less),less)
          , ((less,leq),less)
          , ((less,less),less)
          , ((geq,geq),geq)
          , ((geq,greater),greater)
          , ((greater,geq),greater)
          , ((greater,greater),greater) ] 
   commands .= [make Command "\\card" "card" 1 card_fun]
   quantifiers .= 
        [ ([tex|\qsum|], qsum) ]
          