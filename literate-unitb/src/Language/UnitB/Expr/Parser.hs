module Language.UnitB.Expr.Parser where

import Logic.Expr
import Logic.Expr.Parser.Internal.Setting

import Language.UnitB

import           Control.Lens hiding ( indices )
import           Data.HashMap.Lazy hiding ( map )
import qualified Data.HashMap.Lazy as M

machine_setting :: Machine -> ParserSetting
machine_setting m = setting
        & decls %~ (view' variables m `union`)
        & primed_vars .~ M.mapKeys addPrime (M.map prime $ m!.variables)
    where
        setting = theory_setting (getExpr <$> m!.theory)

schedule_setting :: Machine -> Event -> ParserSetting
schedule_setting m evt = setting & decls %~ ((evt^.indices) `union`)
    where
        setting = machine_setting m 

event_setting :: Machine -> Event -> ParserSetting
event_setting m evt = setting & decls %~ ((evt^.params) `union`)
    where
        setting = schedule_setting m evt
