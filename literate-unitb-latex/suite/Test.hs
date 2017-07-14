{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

    -- Modules
import Latex.Parser 
import Latex.Monad
import Latex.Scanner

    -- Libraries
import Control.Arrow ((&&&))
import Control.Applicative
import Control.Lens
import Control.Monad.State

import Data.Char
import Data.Either.Combinators
import qualified Data.List as L
import qualified Data.HashMap.Lazy as M 
import qualified Data.Maybe as MM
import Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy

import GHC.Generics.Instances

import System.Exit
import System.Process

import Test.UnitTest
import Test.QuickCheck as QC hiding (sized)
import Test.QuickCheck.Instances as T ()
import Test.QuickCheck.RandomTree hiding (size,subtrees)
import Test.QuickCheck.Regression
import Test.QuickCheck.Report

import Text.Printf.TH
import Text.Show.With

import Utilities.Syntactic

path2 :: FilePath
path2 = [path|Tests/sample.tex|]

result2 :: String
result2 = 
     "Right (fromList [(\"align\",[]),(\"calculation\",[Env{calculation} (68),Env{calculation} (29)]),(\"equation\",[]),(\"invariant\",[]),(\"lemma\",[]),(\"machine\",[]),(\"theorem\",[])])"
    

path3 :: FilePath
path3 = [path|Tests/sorted_sequences_err.tex|]

result3 :: String
result3 = L.concat
    [ "Left [Error \"unexpected: }; expected: node; expected: end keyword (equation)\" (LI \"\" 29 13)]"
    ]

path4 :: FilePath
path4 = [path|Tests/sorted_sequences.tex|]

path5 :: FilePath
path5 = [path|Tests/integers.tex|]

sections :: [Text]
sections = [
    "calculation",
    "theorem",
    "equation",
    "align",
    "lemma",
    "invariant",
    "machine"]

extract_structure :: Lazy.Text -> Either [Error] (M.HashMap Text [LatexNode])
extract_structure ct = do
    xs <- latex_structure "" ct
    return (find_env sections xs)

find_env :: [Text] -> LatexDoc -> M.HashMap Text [LatexNode]
find_env kw xs = M.map L.reverse $ L.foldl' f (M.fromList $ L.zip kw $ repeat []) $ contents' xs
    where
        f m (t@(EnvNode (Env _ name _ _ _)))
            | name `elem` kw = M.insertWith (++) name [t] m
            | otherwise        = fold_doc f m t
        f m t                  = fold_doc f m t

test1 :: FilePath -> IO String
test1 path = do
        let f (EnvNode (Env _ n _ doc _))   = Verbatim $ [s|Env{%s} (%d)|] n (L.length $ contents' doc)
            f (BracketNode (Bracket _ _ doc _)) = Verbatim $ [s|Bracket (%d)|] (L.length $ contents' doc)
            f (Text _)            = Verbatim "Text {..}"
        ct <- Lazy.readFile path
        return $ show $ M.map (L.map f) <$> extract_structure ct

test2 :: FilePath -> IO Lazy.Text
test2 path = do
        ct <- Lazy.readFile path
        let x = (do
                tree <- latex_structure path ct
                return (flatten' tree))
        return (case x of
            Right xs -> Lazy.fromStrict xs
            Left msgs -> error $ unpack $ T.unlines $ L.map report msgs)

instance Arbitrary LatexToken where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary LatexDoc where
    arbitrary = sized $ \sz -> makeLatex "foo.txt" <$> aux (ceiling $ fromIntegral sz ** (1.5 :: Float) + 1)
        where
            char = arbitrary `suchThat` \c -> isPrint c && c `L.notElem` ("%{}[]\\" :: String)
            cmd  = char `suchThat` (not.isSpace)
            aux :: Int -> Gen (LatexGen ())
            aux sz = do
                oneof $ L.concat
                    [ [ do
                        name <- listOf1 cmd
                        n    <- choose (0,sz-2)
                        bins <- subtree_size (n+1) sz
                        ts   <- mapM aux (L.tail bins)
                        begin (pack name) ts <$> aux (L.head bins)
                      , brackets <$> aux (sz - 1) ]
                    | sz > 1 ] ++
                    [ text . pack <$> listOf char ]
    shrink = L.map (makeLatex "foo.txt" . texMonad) . sort . subtrees
        where
            sort = L.map (($ ()) . snd) . L.sortOn fst . L.map (sz &&& id)
            sz x = size (x ())

newtype Tokens = TokenStream [(LatexToken,LineInfo)]
    deriving (Generic)

instance Show Tokens where
    show (TokenStream xs) = show xs

instance Arbitrary BracketType where
    arbitrary = QC.elements [Curly,Square]

instance Arbitrary Tokens where
    arbitrary = do
        let true = const True
            li   = LI "foo.txt" 1 1
            char = arbitrary `suchThat` (`L.notElem` ("\\[]{}%" :: String) )
                             `suchThat` (not . isSpace)
        xs <- listOf $ return $ do
            (p,li) <- get
            (x,s') <- lift $ oneof 
                [ do 
                    t <- Open <$> arbitrary <*> pure li
                    return ((t,li),(true,end (t,li)))
                , do 
                    t <- Close <$> arbitrary <*> pure li
                    return ((t,li),(true,end (t,li)))
                , do 
                    t <- TextBlock . pack <$> listOf1 char <*> pure li
                    return ((t,li),(isn't _TextBlock,end (t,li)))
                , do 
                    t <- Command . pack <$> (('\\':) <$> ((:) <$> (char `suchThat` isAlpha)
                                                       <*> listOf (char `suchThat` isAlphaNum))) 
                                        <*> pure li
                    return ((t,li),(isn't _TextBlock,end (t,li)))
                , do 
                    t <- Blank . pack <$> listOf1 (arbitrary `suchThat` isSpace) 
                                      <*> pure li
                    return ((t,li),(isn't _Blank,end (t,li)))
                ] `suchThat` (p.fst.fst)
            put s'
            return x
        ts <- evalStateT (sequence xs) (true,li)
        return $ TokenStream ts
    -- shrink = genericShrink

instance Arbitrary MutatedTokens where
    arbitrary = do
        (important,ts) <- suchThat (do
            ts <- tokens <$> arbitrary
            let p :: Traversal' a b -> (z,(a,w)) -> Maybe (z,(a,w))
                p pr x = x <$ (x^?_2._1.pr)
                begin :: Traversal' LatexToken ()
                begin = _Command . _1 . only "\\begin" 
                end :: Traversal' LatexToken ()
                end = _Command . _1 . only "\\end"
                important = MM.mapMaybe (\x -> p _Open x <|> p _Close x <|> p end x <|> p begin x) $ L.zip [0..] ts
            return (important,ts)) (not . L.null . fst)
        i <- QC.elements $ L.map fst important
        return $ MutatedTokens $ L.take i ts ++ L.drop (i+1) ts

newtype MutatedTokens = MutatedTokens [(LatexToken,LineInfo)]
    deriving (Show)

prop_flatten_parse_inv :: LatexDoc -> Property
prop_flatten_parse_inv t = Right t === latex_structure "foo.txt" (Lazy.fromStrict $ flatten t)

prop_flatten_parse_inv_regression :: Property
prop_flatten_parse_inv_regression = regression prop_flatten_parse_inv $
    [ Doc (LI "foo.txt" 1 1) [Text (TextBlock "\247" (LI "foo.txt" 1 1))] (LI "foo.txt" 1 2) 
    , Doc (LI "foo.txt" 1 1) [Text (TextBlock "=s" (LI "foo.txt" 1 1))] (LI "foo.txt" 1 3) 
    , Doc (LI "foo.txt" 1 1) [EnvNode $ Env (LI "foo.txt" 1 1) "\"" (LI "foo.txt" 1 8) (Doc (LI "foo.txt" 1 10) [] (LI "foo.txt" 1 10)) (LI "foo.txt" 1 15)] (LI "foo.txt" 1 17)
    , Doc (LI "foo.txt" 1 1) [EnvNode $ Env (LI "foo.txt" 1 1) "\202" (LI "foo.txt" 1 8) (Doc (LI "foo.txt" 1 10) [Text (TextBlock "H" (LI "foo.txt" 1 10))] (LI "foo.txt" 1 11)) (LI "foo.txt" 1 16)] (LI "foo.txt" 1 18)
    ]

prop_parse_error :: MutatedTokens -> Bool
prop_parse_error (MutatedTokens ts) = isLeft $ latex_structure "foo.txt" (Lazy.fromStrict $ flatten ts)

prop_makeLatex_latexMonad_inverse :: LatexDoc -> Property
prop_makeLatex_latexMonad_inverse t = t === makeLatex "foo.txt" (texMonad t)

prop_flatten_scan_inv :: LatexDoc -> Property
prop_flatten_scan_inv t = Right (tokens t) === scan_latex "foo.txt" (Lazy.fromStrict $ flatten t)

prop_flatten_scan_inv_regression :: Property
prop_flatten_scan_inv_regression = regression prop_flatten_scan_inv cases
    where
        cases = 
            [ Doc (LI "foo.txt" 1 1) [EnvNode $ Env (LI "foo.txt" 1 1) "\232y\171" (LI "foo.txt" 1 8) (Doc (LI "foo.txt" 1 12) [Text (TextBlock "\181" (LI "foo.txt" 1 12))] (LI "foo.txt" 1 13)) (LI "foo.txt" 1 18)] (LI "foo.txt" 1 22)
            ]

prop_uncomment_inv :: Lazy.Text -> Property
prop_uncomment_inv xs' = xs === uncomment xs
    where
        xs = Lazy.filter (/= '%') xs'

prop_uncomment_inv_regression :: Property
prop_uncomment_inv_regression = regression prop_uncomment_inv cases
    where
        cases =
            [ "\r" ]

prop_line_number_inv :: Text -> Property
prop_line_number_inv xs = unpack xs === L.map fst (line_number "" $ Lazy.fromStrict xs)

prop_line_number_inv_regression :: Property
prop_line_number_inv_regression = regression prop_line_number_inv cases
    where
        cases =
            [ "\n" ]

prop_flatten_scan_inv' :: Tokens -> Property
prop_flatten_scan_inv' (TokenStream toks) = Right toks === scan_latex "foo.txt" (Lazy.fromStrict $ T.concat $ L.map lexeme $ L.map fst toks)

prop_flatten_scan_inv'_regression :: Property
prop_flatten_scan_inv'_regression = regression prop_flatten_scan_inv' cases
    where
        cases = 
            [ TokenStream [(Command "\\v" (LI "foo.txt" 1 1),(LI "foo.txt" 1 1)),(Command "\\Gr8z\200" (LI "foo.txt" 1 3),(LI "foo.txt" 1 3)),(Blank "\r\r" (LI "foo.txt" 1 9),(LI "foo.txt" 1 9))]
            ]

prop_non_empty_parse_error :: MutatedTokens -> Property
prop_non_empty_parse_error (MutatedTokens toks) = isLeft xs ==> L.all (not . T.null . message) (fromLeft' xs)
    where 
        xs = latex_structure "foo.txt" (Lazy.fromStrict $ flatten toks)

satisfies :: Show a => (a -> Bool) -> a -> Property
satisfies p x = counterexample (show x) (p x)

prop_non_empty_scan_error :: Text -> Property
prop_non_empty_scan_error str = satisfies isRight $ scan_latex "foo.txt" (Lazy.fromStrict str)

--prop_counter_example0 :: Bool
--prop_counter_example0 = Right counter0 === counter0'

--prop_counter_example1 :: Bool
--prop_counter_example1 = Right counter1 === counter1'

counter0 :: LatexDoc
counter0 = Doc (LI "foo.txt" 1 1) [EnvNode $ Env (LI "foo.txt" 1 1) "\232y\171" (LI "foo.txt" 1 8) (Doc (LI "foo.txt" 1 12) [Text (TextBlock "\181" (LI "foo.txt" 1 12))] (LI "foo.txt" 1 13)) (LI "foo.txt" 1 18)] (LI "foo.txt" 1 22)
    --Doc (LI "foo.txt" 1 1) [Env (LI "foo.txt" 1 1) "r" (LI "foo.txt" 1 8) (Doc (LI "foo.txt" 1 10) [Text (TextBlock "K" (LI "foo.txt" 1 10))] (LI "foo.txt" 1 11)) (LI "foo.txt" 1 11)] (LI "foo.txt" 1 18)

counter0' :: [(LatexToken,LineInfo)]
counter0' = fromRight' $ scan_latex "foo.txt" (Lazy.fromStrict $ flatten counter0)

counter1 :: [(LatexToken,LineInfo)]
counter1 = [(Close Curly (LI "foo.txt" 1 1),(LI "foo.txt" 1 1)),(TextBlock "Bhb~\US\249c&?" (LI "foo.txt" 1 2),(LI "foo.txt" 1 2)),(Blank "\f\v \v \160\n\f\v" (LI "foo.txt" 1 11),(LI "foo.txt" 1 11)),(TextBlock "8N-P\183JM\249\ESC\138" (LI "foo.txt" 2 3),(LI "foo.txt" 2 3)),(Close Square (LI "foo.txt" 2 13),(LI "foo.txt" 2 13)),(Close Square (LI "foo.txt" 2 14),(LI "foo.txt" 2 14)),(Open Square (LI "foo.txt" 2 15),(LI "foo.txt" 2 15)),(TextBlock "!mKJh\US\233" (LI "foo.txt" 2 16),(LI "foo.txt" 2 16)),(Blank "\n \f\f\n" (LI "foo.txt" 2 23),(LI "foo.txt" 2 23))]
    -- "\\begin{5}{{~B}}\\end{5}"
    --Doc (LI "foo.txt" 1 1) [Env (LI "foo.txt" 1 1) "5" (LI "foo.txt" 1 8) (Doc (LI "foo.txt" 1 10) [Bracket Curly (LI "foo.txt" 1 10) (Doc (LI "foo.txt" 1 11) [Bracket Curly (LI "foo.txt" 1 11) (Doc (LI "foo.txt" 1 12) [Text (TextBlock "~B" (LI "foo.txt" 1 12))] (LI "foo.txt" 1 14)) (LI "foo.txt" 1 14)] (LI "foo.txt" 1 15)) (LI "foo.txt" 1 15)] (LI "foo.txt" 1 16)) (LI "foo.txt" 1 21)] (LI "foo.txt" 1 23)

counter1' :: [(LatexToken,LineInfo)]
counter1' = fromRight' $ scan_latex "foo.txt" (Lazy.concat $ L.map (Lazy.fromStrict . lexeme) $ L.map fst counter1)

--counter2 :: LatexDoc
--counter2 = Doc (LI "foo.txt" 1 1) [Env (LI "foo.txt" 1 1) "l\241V\203" (LI "foo.txt" 1 8) (Doc (LI "foo.txt" 1 13) [Bracket Curly (LI "foo.txt" 1 13) (Doc (LI "foo.txt" 1 14) [Env (LI "foo.txt" 1 14) "\177+" (LI "foo.txt" 1 21) (Doc (LI "foo.txt" 1 24) [] (LI "foo.txt" 1 24)) (LI "foo.txt" 1 24)] (LI "foo.txt" 1 32)) (LI "foo.txt" 1 32),Bracket Curly (LI "foo.txt" 1 33) (Doc (LI "foo.txt" 1 34) [Env (LI "foo.txt" 1 34) "? /s" (LI "foo.txt" 1 41) (Doc (LI "foo.txt" 1 46) [Text (TextBlock "\212Y" (LI "foo.txt" 1 46))] (LI "foo.txt" 1 48)) (LI "foo.txt" 1 48)] (LI "foo.txt" 1 58)) (LI "foo.txt" 1 58),Bracket Curly (LI "foo.txt" 1 59) (Doc (LI "foo.txt" 1 60) [Text (TextBlock "Z\190:" (LI "foo.txt" 1 60))] (LI "foo.txt" 1 63)) (LI "foo.txt" 1 63),Bracket Curly (LI "foo.txt" 1 64) (Doc (LI "foo.txt" 1 65) [Text (TextBlock "i\230*" (LI "foo.txt" 1 65))] (LI "foo.txt" 1 68)) (LI "foo.txt" 1 68),Bracket Curly (LI "foo.txt" 1 69) (Doc (LI "foo.txt" 1 70) [Text (TextBlock "k4" (LI "foo.txt" 1 70))] (LI "foo.txt" 1 72)) (LI "foo.txt" 1 72),Bracket Curly (LI "foo.txt" 1 73) (Doc (LI "foo.txt" 1 74) [Bracket Curly (LI "foo.txt" 1 74) (Doc (LI "foo.txt" 1 75) [Env (LI "foo.txt" 1 75) "j" (LI "foo.txt" 1 82) (Doc (LI "foo.txt" 1 84) [Bracket Curly (LI "foo.txt" 1 84) (Doc (LI "foo.txt" 1 85) [Text (TextBlock "6\240n" (LI "foo.txt" 1 85))] (LI "foo.txt" 1 88)) (LI "foo.txt" 1 88)] (LI "foo.txt" 1 89)) (LI "foo.txt" 1 89)] (LI "foo.txt" 1 96)) (LI "foo.txt" 1 96)] (LI "foo.txt" 1 97)) (LI "foo.txt" 1 97),Bracket Curly (LI "foo.txt" 1 98) (Doc (LI "foo.txt" 1 99) [] (LI "foo.txt" 1 99)) (LI "foo.txt" 1 99),Bracket Curly (LI "foo.txt" 1 100) (Doc (LI "foo.txt" 1 101) [] (LI "foo.txt" 1 101)) (LI "foo.txt" 1 101),Bracket Curly (LI "foo.txt" 1 102) (Doc (LI "foo.txt" 1 103) [Text (TextBlock ")h" (LI "foo.txt" 1 103))] (LI "foo.txt" 1 105)) (LI "foo.txt" 1 105)] (LI "foo.txt" 1 106)) (LI "foo.txt" 1 106)] (LI "foo.txt" 1 116)
----counter2 = "{%\220Y+B}{kk\196}{eX*pi}{\\begin{92>xc}bob\\end{92>xc}}{\\begin{Nr}\229\&4\\end{Nr}}{+#\231\164}}\\end{Q}}}"

--counter2' :: Either [Error] [(LatexToken, LineInfo)]
--counter2' = Right $ tokens counter2

--counter2'' :: Either [Error] [(LatexToken, LineInfo)]
--counter2'' = scan_latex "foo.txt" (flatten counter2)


return []

properties :: (PropName -> Property -> IO (a, Result))
           -> IO ([a], Bool)
properties = $forAllProperties'

cases :: TestCase
cases = test_cases "latex parser" [
    (QuickCheckProps "quickcheck" properties),
    (aCase "sample.tex" (test1 path2) result2),
    (aCase "sorted seq err.tex" (test1 path3) result3),
    (CalcCase "reconstitute sample.tex" 
        (test2 path2) 
        (uncomment <$> Lazy.readFile path2)),
    (CalcCase "reconstitute integers.tex" 
        (test2 path5) 
        (uncomment <$> Lazy.readFile path5)),
    (CalcCase "reconstitute sorted seq.tex" 
        (test2 path4) 
        (uncomment <$> Lazy.readFile path4)) ]

test_case :: TestCase
test_case = cases

main :: IO ()
main = do
    _ <- system "rm actual* expected* po-* log*.z3"
    r <- run_test_cases test_case
    if r
        then exitSuccess
        else exitFailure
