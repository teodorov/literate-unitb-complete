module Latex.Test_Latex_Parser where

import qualified Data.Map as M 

--import Latex.Proof_Parser
import Latex.Parser

import Tests.UnitTest

--import Text.ParserCombinators.ReadP

import Utilities.Format
import Utilities.Syntactic

path1 :: FilePath
path1 = "/Users/simonhudon/Documents/ecole/EventB/eventb/trunk/thesis 2/"
    ++ "progress/unit-b-papers/2013-iFM/source/contribution.tex"

path2 :: FilePath
path2 = "tests/sample.tex"

result2 :: String
result2 = concat ["Right (fromList [",
            "(\"align\",[]),",
            "(\"calculation\",",
                "[Env{calculation} (13),",
                 "Env{calculation} (10)]),",
            "(\"equation\",[]),",
            "(\"invariant\",[]),",
            "(\"lemma\",[]),",
            "(\"machine\",[]),",
            "(\"theorem\",[])",
            "])"]

path3 :: String
path3 = "tests/sorted_sequences_err.tex"

result3 :: String
result3 = "Left [Error \"expected \\\\end{equation}, read '}'\" (29,13)]"

path4 :: String
path4 = "tests/sorted_sequences.tex"

path5 :: String
path5 = "tests/integers.tex"

sections :: [String]
sections = [
    "calculation",
    "theorem",
    "equation",
    "align",
    "lemma",
    "invariant",
    "machine"]

extract_structure :: String -> Either [Error] (M.Map String [LatexDoc])
extract_structure ct = do
    xs <- latex_structure "" ct
    return (find_env sections xs)

test_case :: (String, IO Bool, Bool)
test_case = ("latex parser", cases, True)

cases :: IO Bool
cases = test_cases [
    (Case "sample.tex" (main path2) result2),
    (Case "sorted seq err.tex" (main path3) result3),
    (CalcCase "reconstitute sample.tex" 
        (tests path2) 
        (fmap uncomment $ readFile path2)),
    (CalcCase "reconstitute integers.tex" 
        (tests path5) 
        (fmap uncomment $ readFile path5)),
    (CalcCase "reconstitute sorted seq.tex" 
        (tests path4) 
        (fmap uncomment $ readFile path4)) ]

find_env :: [String] -> [LatexDoc] -> M.Map String [LatexDoc]
find_env kw xs = M.map reverse $ foldl f (M.fromList $ zip kw $ repeat []) xs
    where
        f m (t@(Env name _ _ _))
            | name `elem` kw = M.insertWith (++) name [t] m
            | otherwise        = fold_doc f m t
        f m t                  = fold_doc f m t

main :: FilePath -> IO String
main path = do
        ct <- readFile path
        return $ show $ extract_structure ct

tests :: FilePath -> IO String
tests path = do
        ct <- readFile path
        let x = (do
            tree <- latex_structure path ct
            return (concatMap flatten tree))
        return (case x of
            Right xs -> xs
            Left msgs -> error $ unlines $ map (\(Error xs (LI _ i j)) -> format "error ({0},{1}): {2}" i j xs) msgs)