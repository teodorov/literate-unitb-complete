{-# LANGUAGE TypeFamilies #-}
module Language.UnitB.Parser.PlainText where

    -- Libraries
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Text as T
import Pipes
import Text.Parsec as P hiding ((<|>))

instance Monad m => Stream (Producer t m ()) m t where
    -- type Token (Producer t m ()) = t
    uncons = fmap (either (const Nothing) Just) . next
    -- updatePos = _

data UnitBKeyword = Invariant | Machine | End
data UnitBToken = IncIndent | DecIndent | KW UnitBKeyword | Ident Text

prefixed :: Text -> Prism' Text Text
prefixed pre = prism' (pre <>) (\x -> guard (pre `T.isPrefixOf` x) >> return (T.drop (T.length pre) x))

matchOne :: [(Text,a)] -> Text -> Maybe (Text,a)
matchOne [] _ = Nothing
matchOne ((x,r):xs) ys = (,r) <$> preview (prefixed x) ys <|> matchOne xs ys

fromParser :: Parsec (Producer t m ()) () a -> Consumer Text m a
fromParser p = _

scan :: Monad m => Pipe Text UnitBToken m ()
scan = nextLine 0
    where
        nextLine lvl = do
            ln <- await
            let (margin,ln') = T.span isSpace ln
                lvl' = T.length margin
            if T.null ln' || T.isPrefixOf "--" ln' 
                then nextLine lvl
                else do
                    case compare lvl lvl' of
                        GT -> yield IncIndent
                        LT -> yield DecIndent
                        EQ -> return ()
                    loop lvl' ln'
        loop lvl ln = do
            if T.null ln || T.isPrefixOf "--" ln
                then nextLine lvl
                else do
                    let r = matchOne 
                                [ ("invariant",KW Invariant)
                                , ("machine",KW Machine)
                                , ("end",KW End) ]
                                ln
                    case r of
                        Just (rest,t) -> yield t >> loop lvl rest
                        Nothing -> do
                            let (t,rest) = T.span isAlphaNum ln
                            if T.null t
                                then fail $ unpack $ "invalid input: " <> T.take 5 rest 
                                else yield (Ident t) >> loop lvl rest









