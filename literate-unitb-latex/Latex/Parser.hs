{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
module Latex.Parser where

    -- Modules
import Latex.Scanner 

    -- Libraries
import Control.Applicative hiding ((<|>))
import Control.Arrow
import Control.DeepSeq
import Control.Lens  hiding (argument)
import Control.Monad
import Control.Precondition

import Data.Char
import Data.Either.Combinators
import qualified Data.Foldable as F
-- import Data.List ( intercalate )
import qualified Data.List as L
import Data.Monoid
import Data.Semigroup hiding ( (<>) )
import qualified Data.Semigroup as S 
import Data.String.Lines as LN (linesText')
import Data.Text as T
import Data.Text.Lens as T hiding (Text,_Text)
import Data.Typeable

import GHC.Generics (Generic)

import Safe

-- import System.IO.Unsafe

import Text.Parsec ((<?>),(<|>))
import qualified Text.Parsec as P
import Text.Parsec.Error as P
import qualified Text.Parsec.Pos as P

import Text.Pretty
import Text.Printf.TH

import Utilities.Syntactic hiding (char)

data LatexNode = 
        EnvNode Environment
        | BracketNode Bracket
        | Text LatexToken
    deriving (Eq,Show,Generic,Typeable)

data Environment = Env LineInfo Text LineInfo LatexDoc LineInfo
    deriving (Eq,Show,Generic,Typeable)

data Bracket = Bracket BracketType LineInfo LatexDoc LineInfo
    deriving (Eq,Show,Generic,Typeable)

data LatexDoc = Doc LineInfo [LatexNode] LineInfo
    deriving (Eq,Show,Generic,Typeable)

data BracketType = Curly | Square
    deriving (Eq,Show,Typeable,Generic)

data LatexToken =
        Command Text LineInfo
        | TextBlock Text LineInfo
        | Blank Text LineInfo
        | Open BracketType LineInfo
        | Close BracketType LineInfo
    deriving (Eq, Show, Typeable, Generic)

makePrisms ''LatexToken
makePrisms ''LatexNode
makePrisms ''BracketType

envType :: Environment -> Text
envType (Env _ n _ _ _) = n

--instance Zippable LatexNode where
--    type ZipperImpl LatexNode = MZipperT LatexNode LatexDoc

--instance Zippable LatexDoc where
--    type ZipperImpl LatexDoc = MZipperT LatexDoc LatexNode

instance Semigroup LatexDoc where
    (<>) (Doc li0 xs _) (Doc _ ys li1) = Doc li0 (xs++ys) li1

class Convertible a where
    flatten :: a -> Text

flatten' :: LatexDoc -> Text
flatten' (Doc _ xs _) = T.concat $ L.map flatten xs

instance Convertible LatexDoc where
    flatten = flatten'

instance Convertible LatexNode where
    flatten (EnvNode (Env _ s _ ct _)) = 
               "\\begin{" <> s <> "}"
            <> flatten' ct
            <> "\\end{" <> s <> "}"
    flatten (BracketNode (Bracket b _ ct _)) = openBracket b `T.cons` flatten' ct `T.snoc` closeBracket b
    flatten (Text xs) = lexeme xs

instance Convertible [LatexToken] where
    flatten = T.concat . L.map lexeme

instance Convertible [(LatexToken,LineInfo)] where
    flatten = T.concat . L.map (lexeme.fst)

instance Convertible StringLi where
    flatten (StringLi xs _) = pack $ L.map fst xs

instance IsBracket BracketType Char where
    bracketPair Curly = ('{','}')
    bracketPair Square = ('[',']')

instance PrettyPrintable LatexDoc where
    pretty = unpack . flatten

instance NFData Environment where
instance NFData Bracket where
instance NFData LatexDoc where
instance NFData LatexNode where
instance NFData LatexToken where
instance NFData BracketType where

lineInfoLens :: Lens' LatexToken LineInfo
lineInfoLens f (TextBlock x li) = TextBlock x <$> f li
lineInfoLens f (Command x li) = Command x <$> f li
lineInfoLens f (Blank x li) = Blank x <$> f li
lineInfoLens f (Open x li)  = Open x  <$> f li
lineInfoLens f (Close x li) = Close x <$> f li

whole_line :: LineInfo -> [LineInfo]
whole_line (LI fn i j) = L.map (uncurry3 LI) $ zip3 (repeat fn) (repeat i) [j..]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

flatten_li' :: LatexDoc -> StringLi
flatten_li' (Doc _ xs li) = StringLi (L.concatMap flatten_li xs) li

getString :: StringLi -> [(Char,LineInfo)]
getString (StringLi xs _) = xs

flatten_li :: LatexNode -> [(Char,LineInfo)]
flatten_li (EnvNode (Env li0 s _ ct li1)) = 
           L.zip ("\\begin{" ++ unpack s ++ "}") (whole_line li0)
        ++ getString (flatten_li' ct)
        ++ L.zip ("\\end{" ++ unpack s ++ "}") (whole_line li1)
flatten_li (Text xs)        = lexeme_li xs
flatten_li (BracketNode (Bracket b li0 ct li1)) 
        = (openBracket b,li0) : getString (flatten_li' ct) ++ [(closeBracket b,li1)]

fold_doc :: (a -> LatexNode -> a)
         -> a -> LatexNode -> a
fold_doc f x doc  = L.foldl' f x $ contents' $ contents doc

fold_docM :: Monad m
          => (a -> LatexNode -> m a)
          -> a -> LatexNode -> m a
fold_docM f x doc = foldM f x $ contents' $ contents doc

map_doc :: (LatexNode -> b)
        -> LatexNode -> [b]
map_doc f doc = L.map f $ contents' $ contents doc

map_docM :: Monad m 
         => (LatexNode -> m b)
         -> LatexNode -> m [b]
map_docM f doc = mapM f $ contents' $ contents doc

map_docM_ :: Monad m 
         => (LatexNode -> m b)
         -> LatexNode -> m ()
map_docM_ f doc = mapM_ f $ contents' $ contents doc

isWord :: LatexDoc -> Maybe Text
isWord = fmap fst . isWord'

isWord' :: LatexDoc -> Maybe (Text,LineInfo)
isWord' (Doc _ xs _) = concat' =<< mapM (f <=< preview _Text) xs 
    where
        concat' ((x,li):xs) = Just (x <> T.concat (L.map fst xs),li)
        concat' [] = Nothing
        f (TextBlock x li) = Just (x,li)
        f (Command x li)   = Just (x,li)
        f _                = Nothing

class IsLatexNode node where
    contents :: node -> LatexDoc

instance IsLatexNode Bracket where
    contents (Bracket _ _ c _) = c
instance IsLatexNode Environment where
    contents (Env _ _ _ c _) = c
instance IsLatexNode LatexToken where
    contents t = Doc (line_info t) [] (line_info t)
    -- error? li is actually the beginning of the text. Or is it?

instance IsLatexNode LatexNode where
    contents (EnvNode c)       = contents c
    contents (BracketNode c)   = contents c
    contents (Text t)          = contents t

contents' :: LatexDoc -> [LatexNode]
contents' (Doc _ xs _) = xs

unconsTex :: LatexDoc -> Maybe (LatexNode,LatexDoc)
unconsTex (Doc _ (x:xs) li) = Just (x,Doc (headDef li $ L.map line_info xs) xs li)
unconsTex (Doc _ [] _)      = Nothing

consTex :: LatexNode -> LatexDoc -> LatexDoc
consTex x (Doc _ xs li) = Doc (line_info x) (x:xs) li

prependNodes :: [LatexNode] -> LatexDoc -> LatexDoc
prependNodes ns d = L.foldr consTex d ns

--instance Show LatexNode where
--    show (Env b _ xs _) = "Env{" ++ b ++ "} (" ++ show (length $ contents' xs) ++ ")"
--    show (Text xs _)    = "Text (" ++ show (take 10 xs) ++ "...)"
--    show (Bracket Curly _ c _)  = "Bracket {" ++ show c ++ "} "
--    show (Bracket Square _ c _) = "Bracket [" ++ show c ++ "] "

--instance Show LatexDoc where
--    show (Doc xs _) = show xs

open_bracket :: BracketType -> Char
open_bracket Curly  = '{'
open_bracket Square = '['

close_bracket :: BracketType -> Char
close_bracket Curly  = '}'
close_bracket Square = ']'

instance Syntactic LatexToken where
    line_info (Command _ li)    = li
    line_info (TextBlock _ li)  = li
    line_info (Blank _ li)      = li
    line_info (Open _ li)       = li
    line_info (Close _ li)      = li
    after x = end (x,line_info x)
    traverseLineInfo = lineInfoLens

instance Syntactic LatexNode where
    line_info (EnvNode (Env li _ _ _ _))     = li
    line_info (BracketNode (Bracket _ li _ _)) = li
    line_info (Text t)           = line_info t
    after (Text t) = after t
    after (EnvNode (Env _ nm _ _ li))  = end (nm <> "}",li)
    after (BracketNode (Bracket _ _ _ li)) = end ("}" :: String ,li)
    traverseLineInfo f (BracketNode (Bracket b li0 ct li1)) = fmap BracketNode $
            Bracket b <$> f li0 
                      <*> traverseLineInfo f ct 
                      <*> f li1
    traverseLineInfo f (EnvNode (Env li0 n li1 ct li2)) = fmap EnvNode $
            Env <$> f li0
                <*> pure n
                <*> f li1
                <*> traverseLineInfo f ct
                <*> f li2
    traverseLineInfo f (Text t) = Text <$> lineInfoLens f t

instance Syntactic LatexDoc where
    line_info (Doc li _ _) = li
    after (Doc _ _ li)     = li
    traverseLineInfo f (Doc li xs li') = 
            Doc <$> f li 
                <*> traverse (traverseLineInfo f) xs 
                <*> f li'

tokens :: LatexDoc -> [(LatexToken,LineInfo)]
tokens (Doc _ xs _) = L.concatMap f xs
    where
        f :: LatexNode -> [(LatexToken,LineInfo)]
        f (EnvNode (Env li0 name li1 ct li2)) = f begin ++ cont ++ f end
            where
                f = L.map (id &&& line_info)
                openLI = li0 & column %~ (L.length ("\\begin" :: String) +)
                closeLI = li1 & column %~ (T.length name+)
                endLI = afterLast (closeLI & column %~ (1+)) cont
                openLI' = endLI & column %~ (L.length ("\\end" :: String)+)
                closeLI' = li2 & column %~ (T.length name+)
                begin = [ (Command "\\begin" li0)
                        , (Open Curly openLI)
                        , (TextBlock name li1) 
                        , (Close Curly closeLI)]
                cont = tokens ct
                end  = [ (Command "\\end" endLI)
                       , (Open Curly openLI')
                       , (TextBlock name li2) 
                       , (Close Curly closeLI')]
        f (BracketNode (Bracket br li0 ct li1)) = [(Open br li0,li0)] ++ tokens ct ++ [(Close br li1,li1)]
        f (Text tok) = [(tok,line_info tok)]

source :: LatexNode -> Text
source (Text t) = lexeme t
source x           = pack $ show x

instance Token LatexToken where
    -- lexemeString = show
    lexeme (Command xs _)   = xs
    lexeme (TextBlock xs _) = xs
    lexeme (Blank xs _)     = xs
    lexeme (Open Curly _)   = "{"
    lexeme (Open Square _)  = "["
    lexeme (Close Curly _)  = "}"
    lexeme (Close Square _) = "]"

lexeme_li :: LatexToken -> [(Char, LineInfo)]
lexeme_li x = L.zip (unpack $ lexeme x) $ whole_line li
    where
        li    = line_info x

begin_kw :: Text
end_kw   :: Text

begin_kw = "\\begin"
end_kw   = "\\end"

is_identifier :: String -> Maybe Int
is_identifier t | not $ L.null $ L.filter isAlpha t = Just ln
                | otherwise                         = Nothing
    where ln = L.length (L.takeWhile isAlphaNum t)

is_command :: String -> Maybe Int
is_command ys = case L.uncons ys of
    Nothing -> Nothing
    Just (x,xs)
        | x == '\\'     ->
            (do n <- is_identifier xs
                return (n+1)) `mplus`
            (do guard (not $ L.null xs)
                if isSymbol $ L.head xs
                    then return 2
                    else Nothing)
        | otherwise     -> Nothing

is_space :: String -> Maybe Int
is_space xs = do
        let n = L.length $ L.takeWhile isSpace xs
        guard (1 <= n)
        Just n

scan_latex :: FilePath -> Text -> Either [Error] [(LatexToken,LineInfo)]
scan_latex fn xs = -- trace ([s|input: %s\nuncomment: %s\n|] xs cs) $ 
        -- mapLeft toErr $ P.parse tex_tokens fn $ uncomment xs
        read_lines tex_tokens fn (uncomment xs)
    --where cs = uncomment xs


tex_tokens :: Scanner Char [(LatexToken,LineInfo)]
tex_tokens = do 
    b <- is_eof
    if b
        then return []
        else do
            li <- get_line_info
            c <- match_first [
                    (is_space, \xs -> return $ Just $ Blank (pack xs) li),
                    (is_command, \xs -> return $ Just $ Command (pack xs) li),
                    (match_string "{", (\_ -> return (Just $ Open Curly li))),
                    (match_string "}", (\_ -> return (Just $ Close Curly li))),
                    (match_string "[", (\_ -> return (Just $ Open Square li))),
                    (match_string "]", (\_ -> return (Just $ Close Square li))) ]
                    (return Nothing)
            case c of
                Just x  -> do
                    xs <- tex_tokens
                    return ((x,li):xs)
                Nothing -> do
                    li <- get_line_info
                    d  <- read_char
                    xs <- tex_tokens
                    case xs of
                        (TextBlock ys _,_):zs -> 
                            return ((TextBlock (d `T.cons` ys) li,li):zs)
                        _ ->return ((TextBlock (T.singleton d) li,li):xs)

type Parser = P.Parsec [LatexToken] ()

posToLi :: P.SourcePos -> LineInfo
posToLi pos = LI <$> P.sourceName <*> P.sourceLine <*> P.sourceColumn $ pos

liToPos :: LineInfo -> P.SourcePos
liToPos li = P.newPos (li^.filename) (li^.line) (li^.column)

latex_content' :: Parser LatexDoc
latex_content' = do
        Doc <$> lineInfo
            <*> P.many node
            <*> lineInfo
    where
        node  = (env <|> brackets <|> text) <?> "node"
        env   = do
            li0 <- lineInfo
            cmd "\\begin"          <?> "begin environment"
            (n,li1) <- argument    <?> "argument a"
            ct  <- latex_content'
            cmd "\\end"            <?> [s|end keyword (%s)|] n
            (_,li2) <- argument' n <?> [s|\\end{%s}|] n
            return $ EnvNode $ Env li0 n li1 ct li2
        brackets = do
            li0 <- lineInfo
            b   <- open <?> "open bracket"
            ct  <- latex_content'
            li1 <- lineInfo
            close b  <?> "close bracket"
            return $ BracketNode $ Bracket b li0 ct li1
        text  = fmap Text $ 
                    (texToken' (_Command . notEnv) <?> "command")
                <|> (texToken' _Blank     <?> "space")
                <|> (texToken' _TextBlock <?> "text")
        notEnv :: Applicative f
               => ( (Text,LineInfo) -> f (Text,LineInfo) ) 
               -> (Text,LineInfo) -> f (Text,LineInfo)
        notEnv f (x,li)
            | x `notElem` ["\\begin","\\end" :: Text] = f (x,li)
            | otherwise                       = pure (x,li)
        open     = texToken (_Open._1)
        close b  = texToken (_Close._1.only b)
        argument = argumentAux Nothing
        argument' n = argumentAux (Just n)
        argumentAux :: Maybe Text -> Parser (Text,LineInfo)
        argumentAux kw = do
            _ <- optional $ texToken _Blank
            let 
                (Fold p,argTag) = case kw of
                                Just x ->  (Fold $ filtered (==x),[s|argument: %s|] x)
                                Nothing -> (Fold id,"argument")

            texToken (_Open._1._Curly)    <?> "open curly"
            li  <- lineInfo
            arg <- texToken (_TextBlock._1.p) <?> argTag
            texToken (_Close._1._Curly)       <?> "close curly"
            return (arg,li)
        lineInfo = posToLi <$> P.getPosition
        cmd x  = texToken (_Command._1.only x)

texToken :: (Fold LatexToken a) -> Parser a
texToken = token 

token :: ( Syntactic token,P.Stream xs Identity token
         , Token token)
      => (Fold token a) -> P.Parsec xs () a
token p = tokenAux (^? p)

texTokenAux :: (LatexToken -> Maybe a) -> Parser a
texTokenAux = tokenAux

tokenAux :: ( Syntactic token,P.Stream xs Identity token
            , Token token)
         => (token -> Maybe a) -> P.Parsec xs () a
tokenAux p = P.tokenPrim lexemeString (const (\t -> const $ liToPos $ end (t,line_info t)) . posToLi) p

token' :: ( Syntactic token,P.Stream xs Identity token
          , PrettyPrintable token,Token token)
       => (Traversal' token a) -> P.Parsec xs () token
token' p = tokenAux (\x -> x <$ (x^?p))

texToken' :: (Traversal' LatexToken a) -> Parser LatexToken
texToken' p = texTokenAux (\x -> x <$ (x^?p))

latex_content :: FilePath -> [(LatexToken,LineInfo)] -> (Int,Int) -> Either [Error] LatexDoc
latex_content fn toks (i,j) = mapLeft toErr $ P.parse parser fn $ L.map fst toks
    where
          parser = do
            P.setPosition (P.newPos fn i j)
            x <- latex_content'
            eof
            return x
          eof = P.try (do{ c <- P.try (texToken' id); P.unexpected (lexemeString c) }
                                <|> return ()
                                )

toErr :: ParseError -> [Error]
toErr e = [Error (errMsg e) (posToLi $ P.errorPos e)]
    where 
          errMsg e = T.intercalate "; " $ L.nub $ L.concatMap f $ errorMessages e
          f (SysUnExpect xs) = ["unexpected: " <> pack xs]
          f (UnExpect xs) = ["unexpected: " <> pack xs]
          f (Expect xs)   = ["expected: " <> pack xs]
          f (Message xs)  = [pack xs]


size :: LatexDoc -> Int
size (Doc _ xs _) = sum $ L.map f xs
    where
        f (Text _) = 1
        f (EnvNode (Env _ _ _ ct _)) = 1 + size ct
        f (BracketNode (Bracket _ _ ct _)) = 1 + size ct

subtrees :: LatexDoc -> [() -> LatexDoc]
subtrees (Doc li xs li') = L.concatMap f xs ++ ys
    where
        f (Text _) = []
        f (EnvNode (Env li0 n li1 ct li2)) = (\() -> ct) : L.concat 
            [ [ doc, \() -> Doc li [EnvNode $ Env li0 n li1 (doc ()) li2] li' ] 
                | doc <- subtrees ct ]
        f (BracketNode (Bracket b li1 ct li2)) = (\() -> ct) : 
            L.concat [ 
                [ doc, \() -> Doc li [BracketNode $ Bracket b li1 (doc ()) li2] li' ] 
                    | doc <- subtrees ct ]
        g t@(Text _) = [\() -> Doc li [t] li']
        g (EnvNode (Env li0 n li1 ct li2)) = [\() -> Doc li [EnvNode $ Env li0 n li1 ct li2] li']
        g (BracketNode (Bracket b li1 ct li2)) = [\() -> Doc li [BracketNode $ Bracket b li1 ct li2] li']
        ys
            | L.length xs <= 1 = []
            | otherwise      = L.concatMap g xs


opt_args :: Scanner LatexToken [[LatexNode]]
opt_args = return []

comma_sep :: Text -> [Text]
comma_sep xs 
        | T.null xs = []
        | otherwise = T.strip ys : comma_sep (T.drop 1 zs)
    where
        (ys,zs) = T.break (== ',') xs

-- trim :: [Char] -> [Char]
-- trim xs = reverse $ f $ reverse $ f xs
--     where
--         f = dropWhile isSpace

trim_blank_text' :: LatexDoc -> LatexDoc
trim_blank_text' xs = Doc li0 xs' (lis ! L.length xs')
    where
        Doc li0 ys li = drop_blank_text' xs
        ys' = L.scanr (\x -> (allBlank x &&)) True ys
        xs' = L.map fst $ L.takeWhile (not . snd) $ L.zip ys ys'
        allBlank (Text (Blank _ _)) = True
        allBlank _ = False
        --isBlank (Blank _ _) = True
        --isBlank _ = False
        lis = L.map line_info ys ++ [li]

drop_blank_text :: [LatexNode] -> [LatexNode]
drop_blank_text ( Text (Blank _ _) : ys ) = drop_blank_text ys
--drop_blank_text ( Text (Blank _ _ : xs) li : ys ) = drop_blank_text ( Text xs li : ys )
drop_blank_text xs = xs

drop_blank_text' :: LatexDoc -> LatexDoc
drop_blank_text' doc = maybe doc (uncurry drop_blank_text_aux) $ unconsTex doc
    where
        --drop_blank_text_aux :: 
        --drop_blank_text_aux li ( Text [] _ : ys ) = drop_blank_text_aux li ys
        drop_blank_text_aux (Text (Blank _ _)) ys = maybe ys (uncurry drop_blank_text_aux) $ unconsTex ys
        --drop_blank_text_aux _ ( Text (Blank _ li : xs) li' : ys ) = drop_blank_text_aux li ( Text xs li' : ys )
        drop_blank_text_aux x xs = consTex x xs

skip_blank :: Scanner LatexToken ()
skip_blank = do
        xs <- peek
        case xs of
            (Blank _ _:_) -> do _ <- read_char ; return ()
            _  -> return ()


-- type DocWithHoles = Either (Text,LineInfo) (LatexToken,LineInfo)

-- find_input_cmd :: Scanner LatexToken [DocWithHoles]
-- find_input_cmd = do
--         b <- is_eof
--         if b then return []
--         else do
--             r  <- read_char 
--             case r of
--                 Command "\\input" li -> do
--                     ts <- peek
--                     fn <- case ts of
--                         Blank _ _ 
--                             : Open Curly _ 
--                             : TextBlock fn _
--                             : Close Curly _
--                             : _ -> do
--                                 replicateM_ 4 read_char
--                                 return fn
--                         Open Curly _ 
--                             : TextBlock fn _
--                             : Close Curly _
--                             : _ -> do
--                                 replicateM_ 3 read_char
--                                 return fn
--                         _ -> fail "invalid syntax for \\input command"
--                     rs <- find_input_cmd
--                     return $ Left (fn,li) : rs
--                 _ -> do
--                     rs <- find_input_cmd
--                     return $ Right (r,line_info r) : rs

-- scan_file :: FilePath -> Text 
--           -> Either [Error] [DocWithHoles]
-- scan_file fname xs = do
--         ys <- read_lines tex_tokens fname (uncomment xs)
--         read_tokens find_input_cmd ys (LI fname 1 1)
        
--        read_tokens latex_content ys (1,1)

-- do_while :: Monad m
--          => m Bool -> m ()
-- do_while cmd = do
--         b <- cmd
--         if b then
--             do_while cmd
--         else return ()

latex_structure :: FilePath -> Text -> Either [Error] LatexDoc
latex_structure fn xs = do
        ys <- scan_latex fn xs
        latex_content fn ys (1,1)

is_prefix :: Eq a => [a] -> [a] -> Bool
is_prefix xs ys = xs == L.take (L.length xs) ys

uncomment :: Text -> Text
uncomment xs = F.fold $ fmap removeComment $ linesText' xs
    where
        removeComment = uncurry (<>) . second (T.dropWhile (`notElem` ['\n','\r'])) . T.span ('%' /=)

-- with_print :: Show a => a -> a
-- with_print x = unsafePerformIO (do putStrLn $ show x ; return x)

remove_ref :: Text -> Text
remove_ref = unpacked %~ remove_ref'

remove_ref' :: String -> String
remove_ref' ('\\':'r':'e':'f':'{':xs) = remove_ref' xs
remove_ref' ('}':xs) = remove_ref' xs
remove_ref' (x:xs)   = x:remove_ref' xs
remove_ref' []       = []

addCol :: Int -> LineInfo -> LineInfo
addCol n (LI fn i j) = LI fn i (j+n)

