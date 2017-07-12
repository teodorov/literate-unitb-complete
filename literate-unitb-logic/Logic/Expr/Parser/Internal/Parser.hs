{-# LANGUAGE BangPatterns
        ,RecordWildCards
        ,PatternSynonyms      #-}
module Logic.Expr.Parser.Internal.Parser where

    -- Modules
import Latex.Scanner 
import Latex.Parser  hiding (Close,Open,BracketType(..),Command,Parser,Bracket,token)

import Logic.Expr hiding (recordFields,Field)
import Logic.Expr.Parser.Internal.Monad 
import Logic.Expr.Parser.Internal.Scanner
import Logic.Expr.Parser.Internal.Setting hiding (with_vars)
import Logic.Expr.Printable
import           Logic.Expr.Type as T (Field)
import qualified Logic.Expr.Type as T (Field(..))
import Logic.Expr.TypeChecking
import Logic.Operator

import Logic.Theories.SetTheory

import Utilities.Syntactic

    -- Libraries
import Control.Lens hiding (Context,from)

import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans.Either
import           Control.Precondition

import           Data.Char
import           Data.Either
import           Data.Either.Validation
import           Data.List as L
import           Data.List.NonEmpty as NE 
import           Data.HashMap.Lazy as M hiding ( map )
import qualified Data.HashMap.Lazy as M
import           Data.Semigroup hiding (option)
import qualified Data.Set as S
import           Data.Text (Text,unpack)
import qualified Data.Text as T

import Text.Printf.TH as Printf

import Utilities.EditDistance
import Utilities.Graph as G ((!))

get_context :: Parser Context 
get_context = context `liftM` get_params

get_notation :: Parser Notation
get_notation = notation `liftM` get_params

get_table :: Parser (Matrix Operator Assoc)
get_table = (view struct . notation) <$> get_params

get_vars :: Parser (HashMap Name UntypedExpr)
get_vars = variables `liftM` get_params

with_vars :: [(Name, UntypedVar)] -> Parser b -> Parser b
with_vars vs cmd = do
        x <- get_params
        liftP $ runParserWith (f x) $ do
                cmd
    where
        f s@(Param { .. }) =
                s { variables = M.map Word (M.fromList vs) `M.union` variables }

comma :: Parser ()
comma = read_listP [Comma] >> return ()

colon :: Parser ()
colon = read_listP [Colon] >> return ()

read_listP :: [ExprToken] -> Parser [ExprToken]
read_listP xs = liftP $ read_list xs

brackets :: Bracket -> Parser a -> Parser a
brackets b cmd = do
        _ <- read_listP [Open b]
        x <- cmd
        _ <- read_listP [Close b]
        return x

operator :: Parser Name
operator = do
    x <- liftP read_char
    case x of
        Operator n -> return $ fromText n
        _ -> fail "expecting an operator"

word_or_command :: Parser Name
word_or_command = do
    x <- liftP read_char
    case x of
        Ident xs -> return $ fromText xs
        _ -> fail "expecting an identifier"

from :: [(Name,a)] -> Parser a
from m = attempt $ do
        x <- word_or_command
        case x `L.lookup` m of
                Nothing -> fail ""
                Just x  -> return x

type_t :: Parser Type
type_t = choose_la 
    [ add_context "foo" $ recordType
    , do
        t  <- choiceP 
            [ word_or_command
            , operator ]
            (liftP read_char >>= \c -> fail $ unpack $ "expecting word or command: " <> lexeme c) 
            return
        b1 <- look_aheadP $ read_listP [Open Square]
        ts <- if b1
            then do
                _  <- read_listP [Open Square]
                ts <- sep1P type_t comma
                _  <- read_listP [Close Square]
                return ts
            else return []
        ctx <- get_context
        t <- case get_type ctx t of
            Just s -> do
                unless (L.length ts == typeParams s)
                    $ fail $ [Printf.s|Parameter mismatch. Expecting %d type parameters, received %d.|] 
                        (typeParams s) 
                        (L.length ts)
                return $ Gen s ts
            Nothing -> fail $ unpack ("Invalid sort: '" <> renderText t <> "'")
        b2 <- look_aheadP $ read_listP [ Ident "\\pfun" ]               
        if b2 
        then do
            _  <- maybe 
                (fail $ "Invalid sort: '\\pfun'")
                return
                $ get_type ctx $ fromString'' "\\pfun"
            _  <- read_listP [Ident "\\pfun"]
            t2 <- type_t
            return $ fun_type t t2
        else return t ]

get_type :: Context -> Name -> Maybe Sort
get_type (Context ts _ _ _ _) x = M.lookup x m
    where
        m = M.fromList 
                   [ ([tex|\Int|], IntSort)
                   , ([tex|\Real|], RealSort)
                   , ([tex|\Bool|], BoolSort)]
            `M.union` ts
--        z3_type s@(Sort _ x _) = USER_DEFINED s

vars :: Parser [(Name,Type)]
vars = do
        vs <- sep1P word_or_command comma
        colon
        t  <- type_t
        return (L.map (\x -> (x,t)) vs)     

get_variables' :: HashMap Name Sort
               -> LatexDoc
               -> LineInfo
               -> Either [Error] [(Name, Var)]
get_variables' types cs = 
        get_variables 
            (Context types M.empty 
                M.empty M.empty M.empty)
            cs

get_variables :: Context
              -> LatexDoc
              -> LineInfo
              -> Either [Error] [(Name, Var)]
get_variables ctx = get_variables'' ctx . flatten_li'

get_variables'' :: Context
                -> StringLi
                -> LineInfo
                -> Either [Error] [(Name, Var)]
get_variables'' ctx m li = do
        toks <- read_tokens 
            (scan_expr Nothing) (getString m) li
        xs   <- read_tokens 
            (runParser ctx
                undefined' M.empty vars) 
            toks li
        return $ L.map (\(x,y) -> (x,Var x y)) xs

parse_type :: Context
           -> StringLi
           -> LineInfo
           -> Either [Error] Type
parse_type ctx m li = do
        toks <- read_tokens 
            (scan_expr Nothing) (getString m) li
        read_tokens 
            (runParser ctx
                undefined' M.empty type_t) 
            toks li

unary :: Parser UnaryOperator
unary = do
        n <- get_notation
        choiceP
            (L.map f $ lefts $ n^.new_ops)
            (fail "expecting an unary operator")            
            return
    where
        f op@(UnaryOperator _ tok _) = do
            _ <- read_listP [Operator $ renderText tok]
            return op

oper :: Parser BinOperator
oper = do
        n <- get_notation
        choiceP
            (L.map f $ rights $ n^.new_ops)
            (do
                xs <- liftP peek
                fail $ "expecting a binary operator, read: " 
                    ++ show (L.take 1 xs))            
            return
    where
        f op@(BinOperator _ tok _ _) = do
            _ <- read_listP [Operator $ renderText tok]
            return op

data FunOperator = Domain | Range
    deriving Show

apply_fun_op :: Command -> UntypedExpr -> Parser Term
apply_fun_op (Command _ _ _ fop) x = do
        return $ UE $ fun1 fop x

suggestion :: Name -> HashMap Name Text -> [Text]
suggestion xs m = L.map (\(x,y) -> renderText x <> " (" <> y <> ")") $ toAscList ws
  where
    xs' = T.map toLower $ renderText xs
    p ys _ = 2 * distText xs' ys' <= (T.length xs' `max` T.length ys') + 1
      where
        ys' = T.map toLower $ renderText ys
    ws = M.filterWithKey p m

nameLit :: Parser Field
nameLit = T.Field <$> getToken (_Literal._NameLit) "name literal"

assignTok :: Parser (a -> b -> (a,b))
assignTok = getToken _Assign ":=" >> pure (,)

colonTok :: Parser (a -> b -> (a,b))
colonTok = getToken _Colon ":" >> pure (,)

binding :: Parser (UntypedExpr -> LineInfo -> a) -> Parser (Field,a)
binding = binding' expr

binding' :: Parser term
         -> Parser (term -> LineInfo -> a) 
         -> Parser (Field,a)
binding' term tok = do
    li <- liftP get_line_info
    liftM2 (,) nameLit (tok <*> term <*> pure li)

data Term = 
        Cmd Command 
        | Field Field
        | UE UntypedExpr
    deriving (Show)

instance PrettyPrintable Term where
    prettyText (Cmd c)   = [st|Command: %s|] (prettyText c)
    prettyText (UE ue)   = [st|UntypedExpr:  %s|] (prettyText ue)
    prettyText (Field n) = [st|Field: %s|] (prettyText n)

term :: Parser Term
term = do
    n <- get_notation
    let cmds = L.zip (L.map token (n^.commands)) (n^.commands)
        quants = n^.quantifiers
        oftype = [([tex|\oftype|],())]
    choose_la 
        [ do    c@(Command _ _ n f) <- from cmds
                if n == 1 then do
                    tryP (read_listP [Open Curly])
                        (\_ -> do
                            ue <- expr
                            _  <-read_listP [Close Curly]
                            ue <- return $ fun1 f ue
                            return $ UE ue)
                        (return $ Cmd c)
                else do
                    args <- replicateM n $
                        brackets Curly expr
                    return $ UE $ FunApp f args
        , UE <$> recordSetOrLit
        , do    quant <- from quants 
                ns <- brackets Curly
                    $ sep1P word_or_command comma
                _ctx <- get_context
                let vs :: [UntypedVar]
                    vs = Var <$> ns <*> pure ()
                with_vars (L.zip ns vs) $ do
                    _ <- read_listP [Open Curly]
                    r <- tryP (read_listP [Close Curly]) 
                        (\_ -> return ztrue)
                        (do r <- expr
                            _ <- read_listP [Close Curly]
                            return r)
                    t <- brackets Curly expr
                    let _vars = used_var r `S.union` used_var t
                        ts :: [(Name, UntypedVar)]
                        ts = L.zip ns vs
                        _f = (`S.filter` _vars) . (. view name) . (==)
                    let ts' :: HashMap Name UntypedExpr
                        ts' = M.map Word $ M.fromList ts
                        r' :: UntypedExpr
                        t' :: UntypedExpr
                        r' = substitute' ts' r
                        t' = substitute' ts' t
                        vs' = L.map snd ts
                    UE <$> return (Binder quant vs' r' t' ())
        , do    from oftype
                e <- brackets Curly expr
                t <- brackets Curly type_t
                return $ UE $ Cast Parse e t
        , attempt $ do    
                xs' <- word_or_command
                vs <- get_vars
                case M.lookup xs' vs of
                    Just ue -> return $ UE ue
                    Nothing -> fail ""
        , do    xs <- attempt word_or_command
                vs <- get_vars
                let oftype' = "keyword"  <$ M.fromList oftype
                    quants' = "quantifier" <$ M.fromList quants
                    cmd'    = "command"  <$ M.fromList cmds
                    vars'   = "variable" <$ vs
                    sug     = suggestion xs $ M.unions 
                            [ cmd', quants'
                            , oftype', vars' ]
                fail $ unpack (   "unrecognized term: "
                      <> renderText xs 
                      <> if not $ L.null sug then
                            "\nPerhaps you meant:\n"
                      <> T.intercalate "\n" sug else "")
        , do    xs <- attempt number
                return $ UE $ zint $ read $ unpack xs
        , do    Field <$> nameLit
        ]

recordSetOrLit :: Parser UntypedExpr
recordSetOrLit = do
        _ <- attempt open_square
        choose_la
            [ attempt close_square >> return (Record (RecLit M.empty) (record_type M.empty))
            , do li  <- liftP get_line_info
                 n   <- nameLit
                 sep <- choose_la [True <$ attempt assignTok, False <$ attempt colonTok]
                 ue  <- expr
                 if sep
                    then rec_lit =<< ((n,(ue,li)):) <$> parseTail lit
                    else rec_set =<< ((n,(ue,li)):) <$> parseTail set
            ]
    where
        set = binding colonTok
        lit = binding assignTok
        rec_lit m = Record <$> (RecLit <$> validateFields m)
                           <*> pure ()
        rec_set m = Record <$> (RecSet <$> validateFields m)
                           <*> pure ()
        parseTail field = choose_la 
            [ attempt close_square >> return []
            , do getToken _Comma ","
                 xs <- sep1P field comma
                 _  <- close_square
                 return xs
            ]

validateFields :: [(Field, (expr,LineInfo))] -> Parser (HashMap Field expr)
validateFields xs = raiseErrors $ traverseWithKey f xs'
    where
        xs' = fromListWith (<>) $ xs & mapped._2 %~ pure
        f _ ((x,_):|[]) = Success x
        f k xs = Failure [MLError (msg $ prettyText k) $ xs & mapped._1 .~ " - "]
        msg = [st|Multiple record entry with label '%s'|]
        raiseErrors :: Validation [Error] a -> Parser a
        raiseErrors = either (liftP . Scanner . const . Left) 
                             return . validationToEither

recordType :: Parser Type
recordType = do
        let field = binding' type_t colonTok
            field :: Parser (Field, (Type, LineInfo))
        _  <- attempt open_curly
        xs <- choose_la 
            [ attempt close_curly >> return []
            , do xs <- sep1P field comma
                 _  <- close_curly
                 return xs
            ]
        record_type <$> validateFields xs

recordFields :: Parser (Field,(a,LineInfo)) -> Parser (HashMap Field a)
recordFields field = do
        _  <- attempt open_square
        xs <- choose_la 
            [ attempt close_square >> return []
            , do xs <- sep1P field comma
                 _  <- close_square
                 return xs
            ]
        validateFields xs

-- | \dummy{ x : \Int }
-- | \qforall{y}{}{f.y}
-- | \qforall{f,x}{f.x \le 3}
dummy_types :: [Name] -> Context -> [Var]
dummy_types vs (Context _ _ _ _ dums) = L.map f vs
    where
        f x = fromMaybe (Var x gA) $ M.lookup x dums

number :: Parser Text
number = getToken (_Literal._NumLit) "number"
                
open_square :: Parser [ExprToken]
open_square = read_listP [Open Square]

close_square :: Parser [ExprToken]
close_square = read_listP [Close Square]

open_brack :: Parser [ExprToken]
open_brack  = read_listP [Open Round]

close_brack :: Parser [ExprToken]
close_brack = read_listP [Close Round]

open_curly :: Parser [ExprToken]
open_curly = read_listP [Open QuotedCurly]

close_curly :: Parser [ExprToken]
close_curly = read_listP [Close QuotedCurly]

applyRecUpdate :: [HashMap Field UntypedExpr] -> Term -> Parser Term
applyRecUpdate rUpd (UE ue) = return $ UE $ L.foldl (fmap (`Record` ()) . RecUpdate) ue rUpd
applyRecUpdate xs e@(Cmd op)
        | L.null xs = return e
        | otherwise = fail $ unpack $ "Cannot apply a record update to an operator: " <> prettyText op
applyRecUpdate xs e@(Field op)
        | L.null xs = return e
        | otherwise = fail $ unpack $ "Cannot apply a record update to a record field: " <> prettyText op

expr :: Parser UntypedExpr
expr = do
        r <- read_term []
        case r of
            UE ue -> return ue
            Cmd op -> fail $ unpack $ [st|unapplied functional operator: %s|] (prettyText op)
            Field n -> fail $ unpack $ [st|record field out of context: %s|] (prettyText n)
    where
        read_term :: [([UnaryOperator], Term, BinOperator)] 
                  -> Parser Term
        read_term xs = do
            us <- liftHOF many unary
            choose_la 
                [ do    _  <- attempt open_brack
                        ue <- expr
                        _  <- close_brack
                        rUpd <- manyP (recordFields $ binding assignTok)
                        add_context "parsing (" $
                            read_op xs us =<< applyRecUpdate rUpd (UE ue)
                , do    _  <- attempt open_curly
                        rs <- sep1P expr comma
                        _  <- close_curly
                        ue <- return $ zset_enum' rs
                        add_context "parsing \\{" $
                            read_op xs us $ UE ue
                ,   add_context ("ready for <term>: " <> prettyText xs) $
                        do  t  <- term
                            rUpd <- manyP (recordFields $ binding assignTok)
                            add_context ("parsed <term>: " <> prettyText t) $
                                read_op xs us =<< applyRecUpdate rUpd t
                ]
        read_op :: [([UnaryOperator], Term, BinOperator)] 
                -> [UnaryOperator] 
                -> Term 
                -> Parser Term
        read_op xs us e0 = do
            end <- orM 
                [ liftP $ is_eof
                , look_aheadP close_brack
                , look_aheadP close_curly
                , look_aheadP close_square
                , look_aheadP comma
                , look_aheadP (read_listP [Close Curly]) ]
            if end
            then do
                reduce_all xs us e0
            else do
                op <- oper
                reduce xs us e0 op
        reduce :: [([UnaryOperator], Term, BinOperator)] 
               -> [UnaryOperator]
               -> Term 
               -> BinOperator 
               -> Parser Term
        reduce [] [] e0 op0                 = read_term [([],e0,op0)]
        reduce xs@( (vs,e0,op0):ys ) [] e1 op1 = do
            r <- assoc op0 op1
            case r of
                LeftAssoc ->  do
                    e2 <- apply_op op0 e0 e1
                    reduce ys vs e2 op1
                RightAssoc -> read_term (([],e1,op1):xs)
                NoAssoc ->  fail $ unpack $ [st|ambiguous expression: '%s' and '%s' are not associative|] (prettyText op0) (prettyText op1)
        reduce xs (u:us) e0 op0 = do
            r <- binds u op0
            case r of
                LeftAssoc   -> do
                    e1 <- apply_unary u e0
                    reduce xs us e1 op0
                RightAssoc  -> read_term ((u:us,e0,op0):xs)
                NoAssoc   -> fail ("ambiguous expression: use parentheses")
        reduce_all :: [([UnaryOperator], Term, BinOperator)] 
                   -> [UnaryOperator] 
                   -> Term 
                   -> Parser Term
        reduce_all [] [] e             = return e
        reduce_all ( (us,e0,op0):ys ) [] e1 = do
                e2 <- apply_op op0 e0 e1
                reduce_all ys us e2
        reduce_all xs (u:us) e0        = do
                e1 <- apply_unary u e0
                reduce_all xs us e1
                    
assoc :: BinOperator -> BinOperator -> Parser Assoc
assoc x0 x1 = do
        tb <- get_table
        return $ tb G.! (Right x0, Right x1)

binds :: UnaryOperator -> BinOperator -> Parser Assoc
binds x0 x1 = do
        tb <- get_table
        return $ tb G.! (Left x0, Right x1)

apply_unary :: UnaryOperator -> Term -> Parser Term
apply_unary op e = do
        case e of
            UE ue -> do
                x2 <- return $ mk_unary' op ue
                return $ UE x2
            Cmd oper -> fail $ unpack $ err_msg (prettyText oper) (prettyText op)
            Field n  -> fail $ unpack $ err_msg_2 (prettyText n) (prettyText op)
    where
        err_msg   = [st|functional operator cannot be the operand of any unary operator: %s, %s|]
        err_msg_2 = [st|field names cannot be the operand of any unary operator: %s, %s|]
        
apply_op :: BinOperator -> Term -> Term -> Parser Term
apply_op op x0 x1 = do
        case x1 of
            UE ue1 -> do
                case x0 of
                    UE ue0 -> do
                        ue2 <- return $ mk_expr' op ue0 ue1
                        return $ UE ue2
                    Cmd oper ->
                        if op == apply then
                            apply_fun_op oper ue1
                        else fail $ unpack $ err_msg (prettyText oper) (prettyText op)
                    Field n -> fail $ unpack $ err_msg_2 (prettyText n) (prettyText op)
            Cmd e1  -> 
                fail $ unpack $ err_msg (prettyText e1) (prettyText op)
            Field n 
                | op == apply -> case x0 of
                    UE ue0 -> UE <$> return ((`Record` ()) $ FieldLookup ue0 n)
                    Field n -> fail $ unpack $ err_msg_2 (prettyText n) (prettyText op)
                    Cmd cmd -> fail $ unpack $ err_msg (prettyText cmd) (prettyText op)
                | otherwise   -> fail $ unpack $ err_msg_2 (prettyText n) (prettyText op)
    where
        err_msg = [st|functional operator cannot be the operand of any binary operator: %s, %s|]
        err_msg_2 = [st|field name is not a valid operand: %s, %s|]

parse_expression :: ParserSetting
                 -> StringLi
                 -> Either [Error] UntypedExpr
parse_expression set i@(StringLi input _) = do
        let n = set^.language
            li = line_info i
        toks <- read_tokens (scan_expr $ Just n)
            input li
        ue  <- read_tokens
            (runParser' set expr) 
            toks li
        return ue

parse_oper :: Monad m 
           => Notation
           -> StringLi
           -> EitherT [Error] m BinOperator
parse_oper n s@(StringLi c _) = do
        let li = line_info s
        toks <- hoistEither $ read_tokens 
            (scan_expr $ Just n)
            c li
        !e   <- hoistEither $ read_tokens 
            (runParser empty_ctx n M.empty
                oper) 
            toks li
        return e

parse_expr' :: ParserSetting
            -> LatexDoc
            -> Either [Error] DispExpr
parse_expr' p = parse_expr p . flatten_li' . drop_blank_text'
            
parse_expr :: ParserSetting
           -> StringLi
           -> Either [Error] DispExpr
parse_expr set xs = do
        x <- parse_expression set xs
        typed_x <- checkTypes (set^.expected_type) (contextOf set) x xs
        unless (L.null $ ambiguities typed_x) $ Left 
            $ L.map (\x' -> Error (msg (prettyText x') (prettyText $ type_of x')) (line_info xs))
                $ ambiguities typed_x
        return $ DispExpr (flatten xs) (flattenConnectors typed_x)
    where
        msg   = [st|type of %s is ill-defined: %s|]
