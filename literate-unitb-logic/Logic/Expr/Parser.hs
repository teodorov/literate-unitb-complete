module Logic.Expr.Parser
    ( -- Internal.Parser
      parse_expression, parse_expr, parse_expr'
    , oper, run_test
    , get_variables, get_variables', get_variables''
    , parse_oper, contextOf
      -- Internal.Setting 
    , ParserSetting (..)
    , language, is_step, parserSettingSorts, decls, dum_ctx
    , HasSorts (..)
    , primed_vars, free_dummies, expected_type
    , default_setting, makeSetting, setting_from_context, S.with_vars
    , mkSetting, theory_setting, theory_setting'
    , scan_expr
    , parse_type )
where

import Logic.Expr.Parser.Internal.Parser as P
import Logic.Expr.Parser.Internal.Monad
import Logic.Expr.Parser.Internal.Scanner
import Logic.Expr.Parser.Internal.Setting as S

import Logic.Expr

import Utilities.EditDistance
