{-# LANGUAGE TypeOperators
    ,QuasiQuotes 
    ,TemplateHaskell
    #-}
module Logic.QuasiQuote
    ( module Logic.QuasiQuote
    , Name, InternalName ) 
where

    -- Modules
import Logic.Expr
import Logic.Expr.Parser
import Logic.Expr.Printable
import Logic.Theory

import Logic.Theories

    -- Libraries
import Control.Arrow
import Control.Lens hiding (uncons)
import Control.Monad.State  hiding (lift)

import Data.List
import Data.Map as M
import Data.Maybe
import Data.Monoid
import           Data.Text (Text,unpack,pack)
import qualified Data.Text as T

import Language.Haskell.TH hiding (Name)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax hiding (Name)

import Text.Printf.TH

import Utilities.Syntactic

expr :: QuasiQuoter
expr = QuasiQuoter
    { quoteExp  = \x -> [e| \p -> let loc = $(lift =<< location) in parseExpr loc p $(litE (stringL x)) |]
    , quotePat  = undefined
    , quoteDec  = undefined
    , quoteType = undefined }

var :: QuasiQuoter
var = QuasiQuoter
    { quoteExp  = \x -> [e| let loc = $(lift =<< location) in parseVarDecl loc $(litE (stringL x)) |]
    , quotePat  = undefined
    , quoteDec  = undefined
    , quoteType = undefined }

carrier :: QuasiQuoter
carrier = QuasiQuoter
    { quoteExp  = parseTypeDecl . pack
    , quotePat  = undefined
    , quoteDec  = undefined
    , quoteType = undefined }

field :: QuasiQuoter
field = QuasiQuoter
    { quoteExp  = \str -> [| Field $(quoteExp smt str) |]
    , quotePat  = undefined
    , quoteDec  = undefined
    , quoteType = undefined }

type Parser a = Loc -> ParserSetting -> Text -> a

parseParts :: (a -> b -> c) 
           -> Text
           -> Text
           -> Parser a -> Parser b -> Parser c
parseParts f sep kind pars0 pars1 loc p str | sep `T.isInfixOf` str = f v e
                                            | otherwise             = err
    where
        (rVar,rExpr) = second (T.intercalate sep) $ fromMaybe err $ uncons (T.splitOn sep str)
        v  = pars0 loc p rVar
        e  = pars1 loc p rExpr
        --p' = p & expected_type .~ Just t
        li  = asLI loc
        err = error $ unpack $ "\n" <> show_err [Error ([st|misshapen %s: '%s'|] kind str) li]

parseVarDecl :: Loc -> Text -> State ParserSetting ()
parseVarDecl loc str = do
        ctx <- gets contextOf
        let e  = fromList $ run $ get_variables'' ctx (asStringLi li str) li
            --e' = M.map f e
            --f (Var n t) = Var (S.replace "'" "@prime" n) t
        decls %= M.union e
    where
        li  = asLI loc
        run = either (error.unpack.("\n"<>).show_err) id

parseTypeDecl :: Text -> ExpQ -- State ParserSetting ()
parseTypeDecl str = do
        --str' <- either (fail . unlines) return 
        --    $ isName $ strip str
        --let texName = str'^.asInternal
        --    s = Sort str' texName 0
        let str' = T.strip str
        [e| do
                let t   = set_type $ make_type s []
                    s   = Sort n (asInternal n) 0
                    n   = fromString'' str'
                sorts %= M.insert n s
                decls %= M.insert n (Var n t) |]

primable :: State ParserSetting () -> State ParserSetting ()
primable cmd = do
    s  <- use decls
    cmd
    s' <- use decls
    primed_vars %= M.union (s' `M.difference` s)

parseVar :: Loc -> ParserSetting -> Text -> Var
parseVar loc p str = fromMaybe err $ do
        n' <- isName' n
        M.lookup n' $ p^.decls
    where
        n = T.strip str
        err = error $ unpack $ "\n" <> show_err [Error ([st|unknown variables: '%s'|] n) li]
        li = asLI loc

parseExpr :: Loc -> ParserSetting -> Text -> DispExpr
parseExpr loc p str = either (error.unpack.("\n"<>).show_err) id
            $ parse_expr p (asStringLi li str)
    where li = asLI loc
    -- either fail lift.

type Ctx = (ParserSetting -> DispExpr) -> DispExpr

--impFunctions :: (Text,Theory)
--impFunctions = ("functions",function_theory)

--impSets :: (Text,Theory)
--impSets = ("sets",set_theory)

ctxWith :: [Theory] 
        -> State ParserSetting a 
        -> (ParserSetting -> b) -> b
ctxWith xs cmd f = f r
    where
        r = execState cmd (theory_setting' (symbol_table $ xs ++ M.elems preludeTheories))

ctx :: State ParserSetting a 
    -> (ParserSetting -> b) -> b
ctx = ctxWith []
