{-# LANGUAGE TupleSections,FunctionalDependencies,CPP #-}
module Utilities.Syntactic where

import Control.DeepSeq
import qualified Control.Invariant as I
import Control.Precondition
import Control.Lens

import Control.Monad
import Control.Monad.Trans.Either

import Data.List
import Data.List.NonEmpty as NE (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.List.Ordered
import Data.Semigroup
import Data.Serialize hiding (get)
import Data.Typeable
import qualified Data.String.Lines as L

import GHC.Generics.Instances
import GHC.Read
#if MIN_VERSION_base(4,9,0)
import GHC.Stack
#else
import GHC.SrcLoc
#endif
import GHC.Stack.Utils

import Safe

import Test.QuickCheck as QC
import Test.QuickCheck.ZoomEq

import Text.ParserCombinators.ReadPrec
import Text.Pretty
import Text.Printf.TH


data Error = Error String LineInfo | MLError String (NonEmpty (String,LineInfo))
    deriving (Eq,Typeable,Show,Ord,Read,Generic)

data LineInfo = LI 
        { _filename :: FilePath
        , _line :: Int
        , _column :: Int }     
     deriving (Eq,Ord,Typeable,Generic)

instance Show LineInfo where
    show (LI fn i j) = [s|(LI %s %d %d)|] (show fn) i j

instance Read LineInfo where
    readPrec  = between (string "(LI ") (char ')')
                          $ LI <$> readPrec 
                               <*> (char ' ' >> readPrec)
                               <*> (char ' ' >> readPrec)

between :: ReadPrec () -> ReadPrec () -> ReadPrec a -> ReadPrec a
between first last cmd = do
        first
        x <- cmd
        last
        return x

char :: Char -> ReadPrec ()
char c = do
    c' <- get
    unless (c == c') 
        pfail

string :: String -> ReadPrec ()
string [] = return ()
string (x:xs) = char x >> string xs

makeLenses ''LineInfo

show_err :: [Error] -> String
show_err xs = unlines $ map report $ sortOn line_info xs

class Syntactic a where
    line_info :: a -> LineInfo
    after :: a -> LineInfo
    traverseLineInfo :: Traversal' a LineInfo

class Token t where
    lexeme :: t -> String
    lexemeLength :: t -> LineInfo -> LineInfo
    lexemeLength x
            | length xs <= 1 = column %~ (+ length t)
            | otherwise      = (line %~ (+ (length xs - 1))).(column .~ (length (last xs) + 1))
        where 
            xs = NE.toList $ L.lines t
            t  = lexeme x

instance Token Char where
    lexeme x = [x]

instance Token String where
    lexeme = id

class IsBracket a str | a -> str where
    bracketPair :: a -> (str,str)
    openBracket :: a -> str
    openBracket = fst . bracketPair
    closeBracket :: a -> str
    closeBracket = snd . bracketPair

start :: (a,LineInfo) -> LineInfo
start = snd

end :: Token a => (a,LineInfo) -> LineInfo
end (tok,li) = lexemeLength tok li

afterLast :: Token a => LineInfo -> [(a,LineInfo)] -> LineInfo
afterLast li xs = maybe li end $ lastMay xs

with_li :: LineInfo -> Either [String] b -> Either [Error] b
with_li li = either (\x -> Left $ map (`Error` li) x) Right

instance Syntactic LineInfo where
    line_info = id
    after = id
    traverseLineInfo = id

instance ZoomEq Error where
    (.==) = (I.===)
instance Syntactic Error where
    line_info (Error _ li) = li
    line_info (MLError _ ls) = minimum $ snd <$> ls
    after = line_info
    traverseLineInfo f (Error x li) = Error x <$> f li
    traverseLineInfo f (MLError x lis) = MLError x <$> (traverse._2) f lis

instance ZoomEq LineInfo where
    (.==) = (I.===)
instance Arbitrary LineInfo where
    arbitrary = LI "file" <$> QC.elements [0,5,10] <*> QC.elements [0,5,10]

showLiLong :: LineInfo -> String
showLiLong (LI fn ln col) = [s|%s:%d:%d|] fn ln col

report :: Error -> String
report (Error msg li) = [s|%s:\n    %s|] (showLiLong li) msg
report (MLError msg ys) = [s|%s\n%s|] msg
                (intercalate "\n" 
                    $ map (\(msg,li) -> [s|%s:\n\t%s\n|] (showLiLong li) msg) 
                    $ sortOn snd $ NE.toList ys)

makeReport :: Monad m => EitherT [Error] m String -> m String
makeReport = liftM fst . makeReport' () . liftM (,())

makeReport' :: Monad m => a -> EitherT [Error] m (String,a) -> m (String,a)
makeReport' def m = eitherT f return m
    where    
        f x = return ("Left " ++ show_err x,def)

format_error :: Error -> String
format_error = report

message :: Error -> String
message (Error msg _) = msg
message (MLError msg _) = msg

shrink_error_list :: [Error] -> [Error]
shrink_error_list es' = do
        (xs,e,ys) <- zip3 (inits es) es (drop 1 $ tails es)
        guard $ not $ any (e `less_specific`) $ xs ++ ys
        return e
    where
        less_specific e0@(Error _ _) e1@(Error _ _) = e0 == e1
        less_specific (MLError m0 ls0) (MLError m1 ls1) = m0 == m1 && ls0' `subset` ls1'
            where
                ls0' = sortOn snd $ NE.toList ls0
                ls1' = sortOn snd $ NE.toList ls1
        less_specific _ _ = False
        es = nubSort es'

data TokenStream a = StringLi [(a,LineInfo)] LineInfo
    deriving (Eq,Ord,Show,Functor,Foldable,Traversable,Generic)

type StringLi = TokenStream Char

instance Semigroup (TokenStream a) where
    StringLi xs _ <> StringLi ys li = StringLi (xs <> ys) li
instance ZoomEq a => ZoomEq (TokenStream a) where
instance PrettyPrintable LineInfo where
    pretty (LI _ i j) = [s|(li:%d:%d)|] i j
instance PrettyPrintable a => PrettyPrintable (TokenStream a) where
    pretty str@(StringLi xs _) = pretty (line_info str) ++ ": " ++ pretty (map fst xs)

instance Syntactic (TokenStream a) where
    line_info (StringLi xs li) = headDef li (map snd xs)
    after (StringLi _ li) = li
    traverseLineInfo f (StringLi xs li) = StringLi <$> (traverse._2) f xs <*> f li

instance Arbitrary def => Arbitrary (TokenStream def) where
    arbitrary = genericArbitrary
    shrink = genericShrink

unconsStream :: TokenStream a -> Maybe (a,LineInfo,TokenStream a)
unconsStream (StringLi ((x,li):xs) li') = Just (x,li,StringLi xs li')
unconsStream (StringLi [] _) = Nothing

consStream :: (a,LineInfo)
           -> TokenStream a
           -> TokenStream a
consStream x (StringLi xs li) = StringLi (x:xs) li

stream :: LineInfo -> TokenStream a
stream = StringLi []

neLines :: String -> NonEmpty String
neLines [] = [] :| []
neLines ('\n':xs) = [] :| (y:ys)
    where
        (y :| ys) = neLines xs
neLines (x:xs) = (x:y) :| (ys)
    where
        (y :| ys) = neLines xs

unlinesLi :: NonEmpty StringLi -> StringLi
unlinesLi (x :| []) = x
unlinesLi (StringLi xs0 li0 :| (StringLi xs1 li1:xss)) = unlinesLi $ StringLi (xs0 ++ [('\n',li0)] ++ xs1) li1 :| xss

asStringLi :: LineInfo -> String -> StringLi
asStringLi li xs = unlinesLi ys'
    where
        ys = NE.zip (NE.iterate nxLn li) (neLines xs)
        ys' = NE.map f ys
        nxLn (LI fn i _j) = LI fn (i+1) 1
        nxCol (LI fn i j) = LI fn i (j+1)
        f (x,y) = StringLi (zip y lis) (lis !! length y)
            where lis = iterate nxCol x

asLI :: Loc -> LineInfo
asLI loc = uncurry (LI (loc_filename loc)) (loc_start loc)

mkLI :: String -> LineInfo
mkLI str = LI str 1 1

liLens :: Lens' SrcLoc LineInfo
liLens f loc = fmap update . f $ LI
            (srcLocFile loc) 
            (srcLocStartLine loc)
            (srcLocStartCol loc)
    where
        update (LI fn i j) = loc { srcLocFile = fn
                                 , srcLocStartLine = i
                                 , srcLocStartCol = j }

locToLI :: SrcLoc -> LineInfo
locToLI = view liLens

errorTrace :: I.Pre => [FilePath] -> CallStack -> String -> [Error]
errorTrace fs stack msg = [MLError msg $ nonEmpty' $ loc & mapped._2 %~ locToLI]
    where
        loc = getSrcLocs fs stack

instance PrettyPrintable Error where
    pretty = report

instance NFData Error 
instance NFData LineInfo
instance NFData a => NFData (TokenStream a)
instance Serialize LineInfo where
