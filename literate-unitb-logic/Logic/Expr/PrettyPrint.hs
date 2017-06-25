{-# LANGUAGE 
        TypeOperators,CPP
        ,ScopedTypeVariables
        ,OverloadedStrings #-}
module Logic.Expr.PrettyPrint 
    ( pretty_print', Pretty(..), PrettyPrintable(..) 
    , field
    , prettyRecord, PrettyRecord(..)
    , genericRecordFields )
where

    -- Modules
import Logic.Expr.Classes

    -- Libraries
#if MIN_VERSION_lens(4,13,0)
import Control.Lens hiding (List,cons,uncons)
#else
import Control.Lens hiding (cons,uncons)
#endif

import Control.Monad.Reader

import Data.DList as D (DList)
import qualified Data.DList as D
import Data.DList.Utils as D
import Data.Foldable 
import Data.Monoid
import Data.List as L hiding (uncons,unlines)
import           Data.Text (Text)
import qualified Data.Text as T

import Prelude hiding (unlines)

import Text.Pretty

pretty_print' :: Tree t => t -> Text
pretty_print' t = fold $ D.intersperse "\n" 
    $ map toString $ as_list $ fst 
    $ runReader (pretty_print_aux $ as_tree t) mempty

data Line = Line String' String'
-- newtype Line = Line String'

toString :: Line -> DList Text
toString (Line (String' xs _) (String' ys _)) = xs <> ys

line :: Line -> String'
line (Line _ ys) = ys

data String' = String' (DList Text) !Int
type M = Reader String'
type X = (List Line,Int)

data List a = Ls (DList a) a

instance Monoid String' where
    mappend (String' xs n) (String' ys m) = String' (xs <> ys) (n + m)
    mempty = String' mempty 0

lengthS :: String' -> Int
lengthS (String' _ n) = n

singletonS :: Text -> String'
singletonS xs = String' (D.singleton xs) (T.length xs)

asText :: Iso' String' Text
asText = iso (\(String' x _) -> fold x) singletonS

appendL :: List a -> List a -> List a
appendL (Ls xs x) (Ls ys y) = Ls (xs <> D.cons x ys) y

tell' :: String' -> M X
tell' xs = do
    ys <- ask
    return $ (Ls D.empty $ Line ys xs, lengthS xs+1)

appendall :: [X] -> X
appendall ((x0,n):(x1,m):xs) = appendall $ (appendL x0 x1, n+m) : xs
appendall [x] = x
appendall _ = error "appendall: empty list"

cons :: a -> [a] -> List a
cons x xs = Ls (D.fromList $ init ys) (last ys)
    where
        ys = x:xs

uncons :: List a -> (a -> [a] -> b) -> b
uncons ls f = f (head zs) (tail zs)
    where
        zs = as_list ls

as_list :: List a -> [a]
as_list (Ls xs x) = D.apply xs [x]

pretty_print_aux :: StrList -> M X
pretty_print_aux (Str xs) = tell' $ singletonS xs
pretty_print_aux (List []) = tell' $ singletonS "()"
pretty_print_aux (List ys@(x:xs)) = 
        case x of
            Str y'    -> do
                zz <- mapM pretty_print_aux xs
                let one_line' :: String'
                    one_line' = foldMap (singletonS " " <>) $ L.concatMap (L.map line . as_list . fst) zz
                    k = sum $ map snd zz
                    y = singletonS y'
                if k <= 50
                then tell' $ singletonS "(" <> y <> one_line' <> singletonS ")"
                else do
                    zs <- prefix_first ("(" <> y' <> " ") $
                        mapM pretty_print_aux xs
                    return $ add_to_last ")" $ appendall zs
            List _ -> do
                zs <- prefix_first "( " $
                    mapM pretty_print_aux ys
                return $ add_to_last " )" $ appendall zs
    where
        prefix_first :: Text -> M [X] -> M [X]
        prefix_first xs cmd = do
            let k = T.length xs
            ys <- indent k cmd 
            case ys of
                [] -> (:[]) `liftM` tell' (singletonS xs)
                (ls, n):ys -> 
                    uncons ls $ \(Line m y) zs -> do
                        let newLine = Line (m & asText %~ T.take (lengthS m - k)) (singletonS xs <> y)
                        return $ (cons newLine zs, n+k):ys
        indent :: Int -> M a -> M a
        indent n cmd = do
            local (margin n <>) cmd
        margin n = singletonS $ T.replicate n " "

add_to_last :: Text -> (List Line, t) -> (List Line, t)
add_to_last suff (Ls xs (Line x y),k) = (Ls xs (Line x $ y <> singletonS suff),k)
  
-- pretty_print :: StrList -> [String]
-- pretty_print (Str xs) = [xs]
-- pretty_print (List []) = ["()"]
-- pretty_print (List ys@(x:xs)) = 
--         case x of
--             Str y    -> 
--                 if length one_line <= 50
--                 then ["(" ++ y ++ " " ++ one_line ++ ")"]
--                 else
--                     zipWith (++)
--                         (("(" ++ y ++ " "):repeat (margin (length y + 2)))
--                         (add_to_last ")" zs)
--             List _ -> zipWith (++)
--                 ("( ":repeat (margin 2))
--                 (add_to_last " )" zs')
--     where
--         margin n = replicate n ' '
--         add_to_last suff xs = 
--             case reverse xs of
--                 y:ys -> reverse ( (y++suff):ys )
--                 []        -> [suff]
--         zs = concatMap pretty_print xs
--         zs' = concatMap pretty_print ys
--         one_line = intercalate " " zs
