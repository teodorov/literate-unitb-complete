{-# LANGUAGE OverloadedStrings #-}
module Language.UnitB.Parser.Tests.Puzzle where

    -- Modules
import Language.UnitB.Parser.Tests.Suite

import Documentation.SummaryGen

    -- Library
import Control.Monad
import Control.Precondition ((!))

import           Data.Text (Text)
import qualified Data.Text as T

import Test.UnitTest

test_case :: TestCase
test_case = test_cases 
        "The king and his advisors puzzle"
        [ aCase "puzzle, event visit" case3 result3
        ]

path0 :: FilePath
path0 = [path|Tests/puzzle/puzzle.tex|]

case3 :: IO Text
case3 = liftM (either id id) $ runEitherT $ do
    s <- get_system path0
    let ms  = s!.machines
        m   = ms ! "m1"
        visit = "visit"
        evt   = nonSkipUpwards m ! visit
    return $ getListing $ event_summary' m visit evt

result3 :: Text
result3 = T.unlines
    [ "\\noindent \\ref{visit} $[p]$ \\textbf{event}"
    , "\\begin{block}"
    , "  \\item   \\textbf{during}"
    , "  \\begin{block}"
    , "    \\item[ ]\\sout{$\\false$} %"
    , "    %(\\ref{visit}/default)"
    , "  \\end{block}"
    , "  \\item   \\textbf{begin}"
    , "  \\begin{block}"
    , "    \\item[ ]{$vs \\bcmeq vs \\bunion \\{ p \\}$} %"
    , "    %\\eqref{visitact1}"
    , "  \\end{block}"
    , "  \\item   \\textbf{end} \\\\"
    , "\\end{block}"
    ]
