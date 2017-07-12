{-# LANGUAGE KindSignatures #-}
module Utilities.Language 
  ( module Utilities.Language 
  , pack )
where

import Control.DeepSeq
import Control.Lens hiding (elements)
import Control.Monad
import Control.Monad.Writer hiding ((<>),lift)

import Data.Char
import Data.Data
import Data.Hashable
import Data.List.NonEmpty
import Data.Semigroup hiding (option)
import Data.Serialize
import Data.String
import Data.Text as T
import Data.Text.Encoding as T

import GHC.Generics

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import qualified Text.Parsec as P

import Test.QuickCheck as QC
import Test.QuickCheck.Instances as QC ()

newtype Language a = Language { _language :: forall m. IsLanguage m => m a}
    deriving (Functor)

newtype NEText = NEText { getText :: Text }
    deriving (Data,Generic,Eq,Ord,Show)

class (Monad m) => IsLanguage (m :: * -> *) where
    (<?>) :: m a -> String -> m a
    (<|>) :: m a -> m a -> m a
    anyChar :: m Char
    string :: String -> m String
    many1 :: m a -> m [a]
    many  :: m a -> m [a]
    option :: a -> m a -> m a
    alphaNum :: m Char
    char :: Char -> m Char
    suchThat :: (a -> Bool) -> m a -> m a
    oneOf :: [Char] -> m Char
    satisfy :: (Char -> Bool) -> m Char
    try :: m a -> m a

makeLenses ''Language

--instance Alternative Language where
--instance MonadPlus Language where
instance Applicative Language where
    pure = return
    (<*>) = ap
instance Monad Language where
    return x = Language $ return x
    Language x >>= f = Language $ x >>= _language . f

newtype Gen' a = Gen { getGen :: Gen (NonEmpty (Writer String a)) }
    deriving (Functor)

elements' :: NonEmpty a -> Gen a
elements' = elements . toList

oneof' :: NonEmpty (Gen a) -> Gen a
oneof' = oneof . toList

--instance MonadPlus Gen' where
--instance Alternative Gen' where
instance Applicative Gen' where
    pure = return
    (<*>) = ap
instance Monad Gen' where
    return x = Gen $ return $ (return x) :| []
    Gen xs >>= f = Gen $ do
            (r,w) <- runWriter <$> (elements' =<< xs)
            fmap (fmap (tell w >>)) . getGen . f $ r

instance  P.Stream text Identity Char => IsLanguage (P.Parsec text ()) where
    (<?>) = (P.<?>)
    (<|>) = (P.<|>)
    anyChar = P.anyChar
    string  = P.string
    many1  = P.many1
    many   = P.many
    option = P.option
    alphaNum = P.alphaNum
    oneOf = P.oneOf
    char  = P.char
    satisfy = P.satisfy
    try     = P.try
    suchThat p cmd = cmd >>= (\x -> guard (p x) >> return x)

ne :: a -> NonEmpty a
ne = (:| [])

ne' :: Char -> NonEmpty (Writer String Char)
ne' x = (tell [x] >> return x) :| []

instance IsLanguage Gen' where
    (<?>) = const
    Gen xs <|> Gen ys = Gen $ (<>) <$> xs <*> ys
    anyChar = Gen $ ne' <$> arbitrary
    string str = Gen $ return $ ne $ tell str >> return str
    many1 (Gen cmd) = Gen $ fmap sequence . ne <$> listOf1 (elements' =<< cmd)
    many (Gen cmd)  = Gen $ fmap sequence . ne <$> listOf (elements' =<< cmd)
    option x cmd = return x <|> cmd
    alphaNum = Gen $ fmap ne' $ QC.suchThat arbitrary isAlphaNum
    oneOf xs = Gen $ fmap sconcat $ sequence $ fmap (getGen . char) (fromList xs)
    satisfy p = Gen $ fmap ne' $ QC.suchThat arbitrary p
    suchThat p (Gen x) = Gen $ fmap ne $ flip QC.suchThat (p . fst . runWriter) (elements' =<< x) 
    char c = Gen $ return $ ne' c
    try cmd = cmd

instance IsLanguage Language where
    Language x <?> tag = Language $ x <?> tag
    Language x <|> Language y = Language $ x <|> y
    anyChar  = Language anyChar
    string x = Language $ string x
    option x (Language cmd) = Language $ option x cmd
    many1 (Language cmd) = Language $ many1 cmd
    many (Language cmd)  = Language $ many cmd
    alphaNum = Language $ alphaNum
    oneOf cs = Language $ oneOf cs
    satisfy p = Language $ satisfy p
    char c    = Language $ char c
    try (Language cmd) = Language $ try cmd
    suchThat p (Language cmd) = Language $ Utilities.Language.suchThat p cmd

text :: String -> Language Text
text = fmap pack . string

many1' :: Language a -> Language (NonEmpty a)
many1' cmd = (:|) <$> cmd <*> many cmd

many1Text' :: Language Char -> Language NEText
many1Text' cmd = NEText . pack <$> ((:) <$> cmd <*> many cmd)

parse :: P.Stream text Identity Char
      => Language a -> P.SourceName -> text -> Maybe a
parse cmd src xs = either (const Nothing) Just $ parse' cmd src xs

parse' :: P.Stream text Identity Char
       => Language a -> P.SourceName -> text -> Either P.ParseError a
parse' (Language cmd) = P.parse (cmd >>= \x -> P.eof >> return x)

gen :: Language a -> Gen String
gen (Language (Gen cmd)) = fmap execWriter $ elements' =<< cmd

word :: Language a -> Gen a
word (Language (Gen cmd)) = fmap (fst . runWriter) $ elements' =<< cmd

instance Hashable NEText where
instance Arbitrary NEText where
    arbitrary = NEText <$> (T.cons <$> arbitrary <*> arbitrary)
    shrink (NEText x) = (NEText . T.cons (T.head x)) <$> shrink (T.tail x)
instance Semigroup NEText where
    NEText x <> NEText y = NEText $ x <> y

instance Serialize Text where
  put txt = put $ encodeUtf8 txt
  get     = fmap decodeUtf8 get
instance Serialize NEText where
instance NFData NEText where

instance Lift Text where
    lift t = [e| fromString $(stringE $ unpack t) |]
instance Lift NEText where
    lift (NEText t) = [e| NEText $(lift t) |]
