module Document.Expression where
{-# LANGUAGE BangPatterns #-}

import Data.Char
import Data.List as L
import Data.Map as M hiding ( map )
import qualified Data.Map as M ( map )

import Latex.Scanner
import Latex.Parser

import System.IO.Unsafe

import Z3.Const
import Z3.Def
import Z3.Z3

eat_space :: Scanner Char ()
eat_space = do
--        choice [
--            try eof (\() -> fail "") (return ()),
--            read_if isSpace (\_ -> return ()) (fail "")
----            space_cmd
--            ] (return ())
--              (\() -> eat_space)
        b <- is_eof
        if b
        then return ()
        else read_if isSpace (
                \_ -> eat_space)
                (return ())
--            x:_ <- peek
--            if isSpace x
--            then do
--                read_char
--                eat_space
----            else do
----                b <- look_ahead space_cmd
----                if b
----                then do
----                    space_cmd
----                    eat_space
--            else return ()

space_cmd :: Scanner a ()
space_cmd = return ()

isWord x = isAlphaNum x || x == '_'

word0 :: Scanner Char String
word0 = do
        read_if isWord 
            (\x -> do
                xs <- word0
                return (x:xs))
            (return [])

word = do
        read_if isAlpha
            (\x -> do
                xs <- word0
                return (x:xs))
            (fail "expecting non-empty word")

comma = do
        eat_space
        read_if (',' ==) 
            (\_ -> eat_space)
            (fail "expecting comma (',')")

colon :: Scanner Char ()
colon = do
        eat_space
        read_if (':' ==) 
            (\_ -> eat_space)
            (fail "expecting colon (':')")
            
read_list :: (Eq a,Show a) => [a] -> Scanner a [a]
read_list xs = do
        x <- match (match_string xs) 
        case x of
            Just x -> return x
            Nothing -> fail ("expecting: " ++ show xs)
            
word_or_command =
    read_if (== '\\')
            (\_ -> do
                xs <- word
                return ('\\':xs))
            word

type_t :: Context -> Scanner Char Type
type_t ctx@(Context ts _ _ _ _) = do
        t <- word_or_command
        eat_space
        b <- look_ahead $ read_list "["
        ts <- if b
            then do
                read_list "["
                eat_space
                ts <- sep1 (type_t ctx) comma
                eat_space
                read_list "]"
                eat_space
                return ts
            else return []
        case type_of ctx t of
            Just s -> return $ USER_DEFINED s ts
            Nothing -> fail ("Invalid sort: '" ++ t ++ "'")

type_of :: Context -> String -> Maybe Sort
type_of (Context ts _ _ _ _) x = M.lookup x m
    where
        m = fromList ( 
                   [("\\Int", IntSort), ("\\Real", RealSort), ("\\Bool", BoolSort)])
            `M.union` ts
--                ++ zip (keys ts) (map USER_DEFINED $ map z3_type $ elems ts) )
        z3_type s@(Sort _ x _) = USER_DEFINED s

vars :: Context -> Scanner Char [(String,Type)]
vars ctx = do
        eat_space
        vs <- sep1 word comma
        colon
        t <- type_t ctx
        eat_space       
        return (map (\x -> (x,t)) vs)     

get_variables :: Context -> [LatexDoc] -> Either Error [(String, Var)]
get_variables ctx cs = do
        xs <- read_tokens (vars ctx) m
        return $ map (\(x,y) -> (x,Var x y)) xs
    where
        m = concatMap flatten_li cs

--as_variables :: Context -> LatexDoc -> Either Error [(String, Var)]
--as_variables ctx (Env s _ c _) = do
--        xs <- read_tokens (vars ctx) m
--        return $ map (\(x,y) -> (x,Var x y)) xs
--    where
--        m = concatMap flatten_li c

plus = do
        x <- match $ match_string "+"
        case x of
            Just _ -> return ()
            Nothing -> fail "expecting plus (+)"

fun_app = do
        x <- match $ match_string "."
        case x of
            Just _ -> return ()
            Nothing -> fail "expecting function application (.)"

leq = do
        x <- match $ match_string "\\le"
        case x of
            Just _ -> return ()
            Nothing -> fail "expecting less of equal (\\le)"

times = do
        x <- match $ match_string "\\cdot"
        case x of
            Just _ -> return ()
            Nothing -> fail "expecting times (\\cdot)"

implication = do
        x <- match $ match_string "\\implies"
        case x of
            Just _  -> return ()
            Nothing -> fail "expecting implication (\\implies)"

conjunction = do
        x <- match $ match_string "\\land"
        case x of
            Just _  -> return ()
            Nothing -> fail "expecting conjunction (\\land)"

power = do
        x <- match $ match_string "^"
        case x of
            Just _  -> return ()
            Nothing -> fail "expecting exponential (^)"

membership = 
        read_list "\\in"
--        case x of
--            Just _  -> return ()
--            Nothing -> fail "expecting set membership (\\in)"

set_diff = read_list "\\setminus"

oper = do
        choice [
                (plus >> return Plus),
                (times >> return Mult),
                (implication >> return Implies),
                (conjunction >> return And),
                (leq >> return Leq),
                (power >> return Power),
                (membership >> return Membership),
                (equal >> return Equal),
                (set_diff >> return SetDiff),
                (fun_app >> return Apply) ]
            (fail "expecting an operator")            
            return

            
--        try plus 
--            (\_ -> return Plus) $
--            try times
--                (\_ -> return Mult) $
--                try implication
--                    (\_ -> return Implies) $
--                    try conjunction
--                        (\_ -> return And) $
--                        try leq
--                            (\_ -> return Leq) $
--                            try power
--                                (\_ -> return Power) $
--                                (do equal ; return Equal)

equal = do
        x <- match $ match_string "="
        case x of
            Just _  -> return ()
            Nothing -> fail "expecting equal (=)"

term :: Context -> Scanner Char (Expr)
term ctx = do
        eat_space
        try word_or_command
            (\xs -> do
                (ys,zs) <- read_if (== '\'') 
                    (\x -> return (xs ++ "\'", xs ++ "@prime"))
                    (return (xs,xs))
                eat_space
                case xs `L.lookup` [("\\true",ztrue), ("\\false",zfalse)] of
                    Just e  -> return e 
                    Nothing ->
                        case var_decl xs ctx of
                            Just (Var _ t) -> return (Word $ Var zs t)
                            Nothing -> fail ("undeclared variable: " ++ xs))
            (do 
                xs <- number
                eat_space
                return (Const xs INT))

number :: Scanner Char String
number = do
        xs <- match f
        case xs of
            Just n  -> return n
            Nothing -> fail "expecting a number"
    where
        f x
                | n == 0    = Nothing
                | 0 < n     = Just n
            where
                n = length $ takeWhile isDigit x

--assoc Equal Equal = Ambiguous
--assoc Equal Leq   = Ambiguous
--assoc Leq Equal   = Ambiguous
--assoc Leq Leq     = Ambiguous
--assoc Equal _     = RightAssoc
--assoc _ Equal     = LeftAssoc
--assoc Leq _       = RightAssoc
--assoc _ Leq       = LeftAssoc
--assoc Plus Mult   = RightAssoc
--assoc _ _         = LeftAssoc

open_brack  = do 
        x <- match $ match_string "("
        case x of
            Just _ -> return ()
            Nothing -> fail "expecting a bracket '('"

close_brack = do 
        x <- match $ match_string ")" 
        case x of
            Just _ -> return ()
            Nothing -> fail "expecting a bracket ')'"


open_curly = read_list "\\{"

close_curly = read_list "\\}"

expr :: Context -> Scanner Char Expr
expr ctx = do
        r <- read_term []
        return r
    where
        read_term :: [(Expr, Operator)] -> Scanner Char Expr
        read_term xs = do
            s <- peek
            eat_space
            try open_brack
                (\_ -> do
                        r <- expr ctx
                        close_brack
                        eat_space
                        read_op xs r
                    ) $ (try open_curly 
                             (\_ -> do
                                r <- expr ctx
                                close_curly
                                eat_space
                                read_op xs (zmk_set r)
                            ) $ (do
                                t <- term ctx
                                read_op xs t))
        read_op :: [(Expr, Operator)] -> Expr -> Scanner Char Expr
        read_op xs e0 = do
            b1 <- is_eof
            b2 <- look_ahead close_brack
            b3 <- look_ahead close_curly
            if b1 || b2 || b3
            then do
                reduce_all xs e0
            else do
                op <- oper
                reduce xs e0 op
        reduce :: [(Expr, Operator)] -> Expr -> Operator -> Scanner Char Expr
        reduce [] e0 op0                 = read_term [(e0,op0)]
        reduce xs@( (e0,op0):ys ) e1 op1 = do
            case assoc op0 op1 of
                LeftAssoc ->  
                    reduce ys (mk_expr op0 e0 e1) op1
                RightAssoc -> read_term ((e1,op1):xs)
                Ambiguous ->  fail ("ambiguous expression: use parentheses")
        reduce_all [] e               = return e
        reduce_all xs@( (e0,op0):ys ) e1 = do
                reduce_all ys (mk_expr op0 e0 e1)

parse_expr :: Context -> [(Char, (Int,Int))] -> Either (String, Int, Int) Expr
parse_expr ctx c = read_tokens (expr ctx) c

type Error = (String,Int,Int)