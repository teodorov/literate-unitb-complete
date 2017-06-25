{-# LANGUAGE OverloadedStrings #-}
module Latex.Monad where

    -- Modules
import Latex.Parser hiding (token)

    -- Libraries
import Control.Lens
import Control.Monad.RWS

import Data.Char
import Data.MonoTraversable as M
import Data.Function
import Data.List as L
import Data.List.NonEmpty (nonEmpty,fromList)
import Data.Semigroup hiding ((<>))
import Data.Text as T

import Utilities.Syntactic

newtype LatexGen a = LatexGen { getGen :: RWS Bool [LatexDoc] LineInfo a }
    deriving (Functor,Applicative,Monad)

makeLatex :: FilePath -> LatexGen () -> LatexDoc
makeLatex fn cmd = doc
    where
        (doc,_) = evalRWS (getGen $ getDoc cmd) True (LI fn 1 1)

--next :: Int -> LatexGen ()
--next ln = LatexGen $ do
--    b <- ask
--    if b
--        then line += 1 >> column .= 1
--        else column += ln

updateLI :: Text -> LatexGen ()
updateLI xs = do
    LatexGen $ case L.reverse $ T.lines xs of
        y0:_:ys -> do
            line += L.length ys + 1
            column .= T.length y0 + 1
        _ -> column += T.length xs

--horizontal :: LatexGen a -> LatexGen a
--horizontal (LatexGen cmd) = LatexGen $ local (const False) cmd

begin :: Text -> [LatexGen ()] -> LatexGen a -> LatexGen a
begin name args body = do
    liBegin <- LatexGen get
    updateLI "\\begin{"
    li <- LatexGen get
    updateLI $ name <> "}"
    args <- forM args $ getDoc . brackets
    (x,body) <- getDoc' body
    updateLI $ "\\end{" 
    li'  <- LatexGen get
    updateLI $ name <> "}"
    node $ EnvNode $ Env liBegin name li (sconcat $ fromList $ args ++ [body]) li'
    return x

open :: BracketType -> LatexGen ()
open b = do
    li <- LatexGen get
    updateLI $ T.singleton $ fst $ bracketPair b
    token $ Open b li

close :: BracketType -> LatexGen ()
close b = do
    li <- LatexGen get
    updateLI $ T.singleton $ snd $ bracketPair b
    token $ Close b li

brackets :: LatexGen a -> LatexGen a
brackets = brackets' Curly

brackets' :: BracketType -> LatexGen a -> LatexGen a
brackets' b body = do
    li  <- LatexGen get
    updateLI "{"
    (x,doc) <- getDoc' body
    li' <- LatexGen get
    updateLI "}"
    node $ BracketNode $ Bracket b li doc li'
    return x

node :: LatexNode -> LatexGen ()
node n = LatexGen $ get >>= \li -> tell [Doc (line_info n) [n] li]

texMonad :: LatexDoc -> LatexGen ()
texMonad (Doc _ xs _) = mapM_ f xs
    where
        f (Text t) = text (lexeme t)
        f (EnvNode (Env _ n _ ct _)) = begin n [] $ texMonad ct
        f (BracketNode (Bracket b _ ct _)) = brackets' b $ texMonad ct

token :: LatexToken -> LatexGen () 
token tok = do
    node $ Text tok

getDoc' :: LatexGen a -> LatexGen (a,LatexDoc)
getDoc' (LatexGen cmd) = LatexGen $ do
        (x,xs) <- censor (const []) $ listen cmd
        li <- get
        return (x,maybe (Doc li [] li) sconcat $ nonEmpty xs)

getDoc :: LatexGen a -> LatexGen LatexDoc
getDoc = fmap snd . getDoc'

command :: Text -> [LatexGen ()] -> LatexGen ()
command name args = do
    let name' 
            | T.take 1 name == "\\" = name
            | otherwise             = "\\" <> name
    li <- LatexGen get
    updateLI name'
    token $ Command name' li
    forM_ args brackets

text :: Text -> LatexGen ()
text xs = do
    let addNL :: [Text] -> [Text]
        addNL xs = L.map (<> "\n") (L.take (L.length xs - 1) xs) ++ L.drop (L.length xs - 1) xs
    forM_ (addNL $ T.lines xs) $ \ln -> do
        let ys = T.groupBy eq ln
            eq = (==) `on` isSpace
        ys <- forM ys $ \w -> do
            li <- LatexGen get
            LatexGen $ column += T.length w 
            return $ if T.any isSpace w 
                then Blank w li
                else TextBlock w li
        when ('\n' `M.oelem` ln) $ do
            LatexGen $ line += 1 
            LatexGen $ column .= 1 
        mapM_ (node . Text) ys

