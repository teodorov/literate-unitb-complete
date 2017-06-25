{-# LANGUAGE ScopedTypeVariables, QuasiQuotes, TemplateHaskell    #-}
module Data.String.Indentation 
    (Indentation(..),reindent,reindentText,parse',display)
where

import Control.Arrow
import Control.Lens hiding ((<|),List)
import Control.Monad.Reader

import Data.Char
import Data.Foldable as F (all)
import           Data.List.NonEmpty ((<|),NonEmpty(..),toList)
import qualified Data.List.NonEmpty as NE
import Data.Monoid
import Data.Proxy
import           Data.Text (Text,pack,unpack)
import qualified Data.Text as T

import Text.Parsec
import Text.Parsec.Pos
import Text.Printf.TH

class MonadReader r m => Indentation r m | m -> r where
    _margin :: Proxy m -> Lens' r Int
    margin :: m Int
    indent :: Int -> m a -> m a
    mk_lines :: Text -> m [Text]
    margin_string :: m Text

    margin = asks $ view (_margin (Proxy :: Proxy m))
    indent n cmd = local (over (_margin (Proxy :: Proxy m)) (n+)) cmd
    mk_lines "" = return [""]
    mk_lines xs = do
        m <- margin_string
        return $ map (m <>) $ T.lines xs
    margin_string = do
        n <- margin 
        return $ T.replicate n " "

instance Indentation Int (Reader Int) where
    _margin = (\_ -> id)

data Record = 
        Node Text (NE.NonEmpty (Text,Record)) 
        | Field Text [Record]
        | Tuple Text [Record]
        | List Text [Record]
    deriving (Show)

data RecordToken = 
        OpenCurly | CloseCurly 
        | OpenSquare | CloseSquare 
        | OpenRound | CloseRound
        | Equals
        | Comma | Text (NE.NonEmpty Char)
    deriving (Show)

makePrisms ''RecordToken

scan' :: String -> [RecordToken]
scan' = filter p . scan
    where
        p (Text xs) = not $ F.all isSpace xs
        p _ = True

scan :: String -> [RecordToken]
scan [] = []
scan (',':xs) = Comma : scan xs
scan ('{':xs) = OpenCurly : scan xs
scan ('}':xs) = CloseCurly : scan xs
scan ('(':xs) = OpenRound : scan xs
scan (')':xs) = CloseRound : scan xs
scan ('=':xs) = Equals : scan xs
scan ('[':xs) = OpenSquare : scan xs
scan (']':xs) = CloseSquare : scan xs
scan ('"':xs) = uncurry (:) $ ((Text.('"':|)) *** scan) $ scanString xs
scan (x:xs) = case scan xs of 
                Text cs:ys -> Text (x <| cs):ys
                ys -> Text (x :| []) : ys

scanString :: String -> (String,String)
scanString ('"':xs) = ("\"",xs)
scanString ('\\':'"':xs) = first ("\\\"" ++) $ scanString xs
scanString ('\\':'\\':xs) = first ("\\\\" ++) $ scanString xs
scanString (x:xs) = first (x:) $ scanString xs
scanString [] = ([],[])

--newtype Parser a = Parser (StateT [RecordToken] Maybe a)
--    deriving (Functor,Applicative,Alternative,Monad,MonadPlus)

type Parser = Parsec [RecordToken] Int

token' :: Prism' RecordToken a -> Parser a
token' pr = do
        i <- getState
        x <- token show (li i) (^? pr) --Parser $ StateT f
        putState (i+1)
        return x
    where
        --f [] = Nothing
        --f (x:xs) = (,xs) <$> (x^?pr)
        li i _ = newPos "input" 1 i

reindent :: String -> String
reindent xs = concatMap f xs
    where
        f ',' = "\n,"
        f '[' = "\n[ "
        f '{' = "\n{ "
        f '(' = "\n( "
        f x = [x]

reindentText :: Text -> Text
reindentText xs = T.concatMap f xs
    where
        f ',' = "\n,"
        f '[' = "\n[ "
        f '{' = "\n{ "
        f '(' = "\n( "
        f x = T.singleton x
    --fromRight xs $ do
    --display <$> parse' xs

display :: Record -> Text
display r = uncurry [st|%s\n%s|] $ runReader (displayAux r) 0

displayAux :: Record -> Reader Int (Text,Text)
displayAux (Node ns (f:|fs)) = do
    let putField :: Text -> (Text,Record) -> Reader Int Text
        putField str (n,obj) = do
            (h,rec) <- indent 4 $ displayAux obj
            (<> [st|%s%s=%s\n%s|] str n h rec) <$> margin_string
    rest <- mconcat.concat <$> indent 2 (sequence
        [ sequence [putField "{ " f]
        , mapM (putField ",") fs 
        , sequence [ (<> "}\n") <$> margin_string ]
        ])
    return (ns,rest)
displayAux (Field str xs) = do
        let putField obj = do
                (h,rec) <- indent 2 $ displayAux obj
                (<> [st|%s\n%s|] h rec) <$> margin_string
        rest <- mconcat <$> indent 2 (mapM putField xs)
        return (str,rest)
displayAux (List n xs) = do
    case xs of
        [] -> return ([st|%s[]|] n,"")
        (x:xs) -> do
            let putField :: Text -> Record -> Reader Int Text
                putField str obj = do
                    (h,rec) <- indent 2 $ displayAux obj
                    (<> [st|%s %s\n%s|] str h rec) <$> margin_string
            rest <- mconcat.concat <$> indent 2 (sequence
                [ sequence [putField "[ " x]
                , mapM (putField ",") xs 
                , sequence [ (<> "]\n") <$> margin_string ]
                ])
            return (n,rest)
displayAux (Tuple n xs) = do
    case xs of
        [] -> return ([st|%s()|] n,"")
        (x:xs) -> do
            let putField :: Text -> Record -> Reader Int Text
                putField str obj = do
                    (h,rec) <- indent 2 $ displayAux obj
                    (<> [st|%s %s\n%s|] str h rec) <$> margin_string
            rest <- mconcat.concat <$> indent 2 (sequence
                [ sequence [putField "(" x]
                , mapM (putField ",") xs 
                , sequence [ (<> ")\n") <$> margin_string ]
                ])
            return (n,rest)

--runParser :: Parser a -> [RecordToken] -> Maybe a
--runParser = _ -- (Parser cmd) xs = do
    --(x,xs) <- runStateT cmd xs
    --guard (null xs)
    --return x

string' :: Parser Text
string' = pack . toList <$> token' _Text

record :: Parser Record
record = laRecord >> do
    name <- token' _Text <?> "string"
    () <- token' _OpenCurly <?> ("curly: " ++ toList name)
    x  <- field <?> show ()
    xs <- many $ do
        token' _Comma
        field <?> show x
    () <- token' _CloseCurly
    return $ Node (pack $ toList name) $ x :| xs

field :: Parser (Text,Record)
field = do
    name <- string'
    ()   <- token' _Equals
    obj  <- object
    return (name,obj)

object :: Parser Record
object =    (record <?> "record")
        <|> (list <?> "list")
        <|> (tuple <?> "tuple")
        <|> (adt <?> "ADT")
        <|> (Field <$> string' <*> pure []) <?> "<string>"

la :: Parser a -> Parser a
la = try . lookAhead

laRecord :: Parser ()
laRecord = la $ do
    _ <- string'
    token' _OpenCurly

adt :: Parser Record
adt = laRecord >> do
    n  <- string' <?> "name"
    xs <- many $ 
        --(Field <$> string' <*> pure [] <?> "unbracket") 
        (tuple <?> ("bracket: " ++ unpack n))
    x  <- Field <$> string' <*> pure [] 
    return $ Field n $ xs ++ [x]

laList :: Parser ()
laList = la $ do
    _ <- option "" string'
    token' _OpenSquare

list :: Parser Record
list = laList >> do
    n  <- option "" string'
    () <- token' _OpenSquare
    xs <- sepBy (object <?> "object") (token' _Comma <?> "comma") <?> ("list of objects: '" ++ unpack n ++ "'")
    () <- token' _CloseSquare
    return $ List n xs

laTuple :: Parser ()
laTuple = la $ do
    _ <- option "" string'
    token' _OpenRound

tuple :: Parser Record
tuple = laTuple >> do
    n  <- option "" string'
    () <- token' _OpenRound <?> "round: " ++ unpack n
    xs <- sepBy object (token' _Comma)
    () <- token' _CloseRound
    return $ Tuple n xs

parse' :: String -> Either ParseError Record
parse' = runParser object 1 "" . scan'

