{-# LANGUAGE OverloadedStrings #-}
module Documentation.Test where

    -- 
    -- Modules
    --
import Document.Tests.Suite as S

import Documentation.SummaryGen

    --
    -- Libraries
    -- 
import Data.Map as M
import Data.Map.Syntax
import Data.Maybe 

import Tests.UnitTest

import Utilities.FileSystem
import Utilities.Partial
import Utilities.Syntactic

test_case :: TestCase
test_case = test_cases 
        "Documentation generation" 
        [ Case "m2, event m1:moveout" case0 result0
        , Case "m3, event m1:moveout" case1 result1
        , Case "safety properties of m2" case2 result2
        , Case "progress properties of m2" case3 result3
        , Case "File structure" case4 result4
        ]

result0 :: String
result0 = unlines
    [ "\\noindent \\ref{m1:moveout} [t] \\textbf{event}"
    , "\\begin{block}"
    , "  \\item   \\textbf{during}"
    , "  \\begin{block}"
    , "  \\item[ \\eqref{m1:moveoutc1} ]{$t \\in in \\land loc.t \\in plf$} %"
    , "  \\end{block}"
    , "  \\item   \\textbf{upon}"
    , "  \\begin{block}"
    , "  \\item[ \\eqref{m1:moveoutmo:f0} ]{$\\neg ext \\in \\ran.loc$} %"
    , "  \\end{block}"
    , "  \\item   \\textbf{when}"
    , "  \\begin{block}"
    , "  \\item[ \\eqref{m1:moveoutmo:g1} ]{$t \\in in$} %"
    , "  \\item[ \\eqref{m1:moveoutmo:g2} ]{$loc.t \\in plf$} %"
    , "  \\item[ \\eqref{m1:moveoutmo:g3} ]{$\\neg ext \\in \\ran.loc$} %"
    , "  \\end{block}"
    , "  \\item   \\textbf{begin}"
    , "  \\begin{block}"
    , "  \\item[ \\eqref{m1:moveouta2} ]{$loc \\bcmsuch loc' = loc \\1 | t \\fun ext$} %"
    , "  \\end{block}"
    , "  \\item   \\textbf{end} \\\\"
    , "\\end{block}"
    ]

case0 :: IO String
case0 = makeReport $ do
    m <- parse_machine' path0 2
    let lbl = "m1:moveout"
    evt <- S.lookup lbl $ nonSkipUpwards m
    return $ getListing $
            event_summary' m lbl evt

path0 :: String
path0 = "Tests/train-station-set.tex"

result1 :: String
result1 = unlines
    [ "\\noindent \\ref{m1:moveout} [t] \\textbf{event}"
    , "\\begin{block}"
    , "  \\item   \\textbf{during}"
    , "  \\begin{block}"
    , "  \\item[ \\eqref{m1:moveoutc1} ]{$t \\in in \\land loc.t \\in plf$} %"
    , "  \\item[ \\eqref{m1:moveoutm3:mo:sch0} ]{$loc.t \\in osgn$} %"
    , "  \\end{block}"
    , "  \\item   \\textbf{upon}"
    , "  \\begin{block}"
    , "  \\item[ \\eqref{m1:moveoutmo:f0} ]\\sout{$\\neg ext \\in \\ran.loc$} %"
    , "  \\end{block}"
    , "  \\item   \\textbf{when}"
    , "  \\begin{block}"
    , "  \\item[ \\eqref{m1:moveoutmo:g3} ]\\sout{$\\neg ext \\in \\ran.loc$} %"
    , "  \\end{block}"
    , "  \\begin{block}"
    , "  \\item[ \\eqref{m1:moveoutm3:mo:grd0} ]{$loc.t \\in osgn$} %"
    , "  \\item[ \\eqref{m1:moveoutmo:g1} ]{$t \\in in$} %"
    , "  \\item[ \\eqref{m1:moveoutmo:g2} ]{$loc.t \\in plf$} %"
    , "  \\end{block}"
    , "  \\item   \\textbf{begin}"
    , "  \\begin{block}"
    , "  \\item[ \\eqref{m1:moveouta2} ]{$loc \\bcmsuch loc' = loc \\1 | t \\fun ext$} %"
    , "  \\item[ \\eqref{m1:moveoutm3:mo:act0} ]{$isgn,osgn \\bcmsuch isgn' = isgn$} %"
    , "  \\item[ \\eqref{m1:moveoutm3:mo:act1} ]{$isgn,osgn \\bcmsuch osgn'  \\2 = osgn  %"
    , "  \t\\setminus \\{ loc.t \\}$} %"
    , "  \\end{block}"
    , "  \\item   \\textbf{end} \\\\"
    , "\\end{block}"
    ]

case1 :: IO String
case1 = makeReport $ do
    m <- parse_machine' path0 3
    let lbl = "m1:moveout"
    evt <- S.lookup lbl $ nonSkipUpwards m
    return $ getListing $
            event_summary' m lbl evt

result2 :: String
result2 = unlines
    [ "\\textbf{safety}"
    , "\\begin{block}"
    , "\\item[ \\eqref{m2:saf1} ]{$t \\in in \\land loc.t = ext \\textbf{\\quad unless \\quad} \\neg ext \\in \\ran. loc$} %"
    , "\\item[ \\eqref{m2:saf2} ]{$t \\in in \\land b \\in plf \\1\\land  loc.t = b \\textbf{\\quad unless \\quad} b \\in plf \\1\\land \\qforall{t}{ t \\in in }{ \\neg loc.t = b }$} %"
    , "\\end{block}"
    ]

case2 :: IO String
case2 = makeReport $ do
    p <- view' props <$> parse_machine' path0 2
    return $ getListing $
        safety_sum p

result3 :: String
result3 = unlines
    [ "\\textbf{progress}"
    , "\\begin{block}"
    , "\\item[ \\eqref{m2:prog0} ]{$\\true \\quad \\mapsto\\quad \\neg plf \\subseteq \\ran.loc$} %"
    , "\\item[ \\eqref{m2:prog1} ]{$\\true \\quad \\mapsto\\quad \\neg ext \\in \\ran.loc$} %"
    , "\\item[ \\eqref{m2:prog2} ]{$ext \\in \\ran. loc \\quad \\mapsto\\quad \\neg ext \\in \\ran. loc$} %"
    , "\\item[ \\eqref{m2:prog3} ]{$t \\in in \\land loc.t = ext \\quad \\mapsto\\quad \\neg ext \\in \\ran. loc$} %"
    , "\\item[ \\eqref{m2:prog4} ]{$plf \\subseteq \\ran.loc \\!\\! \\quad \\mapsto\\quad \\!\\! \\neg ~ plf \\subseteq \\ran.loc$} %"
    , "\\item[ \\eqref{m2:prog5} ]{$\\qexists{b}{b \\in plf}{ b \\in \\ran.loc } \\quad \\mapsto\\quad \\qexists{b}{}{ b \\in plf \\1\\land \\neg b \\in \\ran.loc }$} %"
    , "\\item[ \\eqref{m2:prog6} ]{$\\qexists{t}{t \\in in}{ b \\in plf \\1\\land  loc.t = b } \\quad \\mapsto\\quad b \\in plf \\land \\neg b \\in \\ran.loc$} %"
    , "\\item[ \\eqref{m2:prog7} ]{$t \\in in \\land b \\in plf \\1\\land  loc.t = b \\quad \\mapsto\\quad b \\in plf \\land \\neg b \\in \\ran.loc$} %"
    , "\\end{block}"
    ]

case3 :: IO String
case3 = makeReport $ do
    m <- parse_machine' path0 2
    return $ getListing $
        liveness_sum m

result4 :: Either String (Map FilePath Bool)
result4 = Right $ fromRight' $ runMap $ do
        "." ## False
        "/" ## False
        "dir" ## False 
        "dir/file" ## False 
        "dir/file/m0_m0-enter.tex"   ## True
        "dir/file/m0_m0-leave.tex"   ## True 
        "dir/file/m1_m0-enter.tex"   ## True
        "dir/file/m1_m0-leave.tex"   ## True 
        "dir/file/m1_m1-moveout.tex" ## True
        "dir/file/m1_m1-movein.tex"  ## True
        "dir/file/m2_m0-enter.tex"   ## True
        "dir/file/m2_m0-leave.tex"   ## True 
        "dir/file/m2_m1-moveout.tex" ## True
        "dir/file/m2_m1-movein.tex"  ## True
        "dir/file/m3_m0-enter.tex"   ## True
        "dir/file/m3_m0-leave.tex"   ## True 
        "dir/file/m3_m1-moveout.tex" ## True
        "dir/file/m3_m1-movein.tex"  ## True
        "dir/file/m3_m3-ctr-plf.tex" ## True
        "dir/file/machine_m0.tex" ## True
        "dir/file/machine_m0_co.tex"    ## True
        "dir/file/machine_m0_inv.tex"   ## True
        "dir/file/machine_m0_prog.tex"  ## True
        "dir/file/machine_m0_props.tex" ## True
        "dir/file/machine_m0_saf.tex"   ## True
        "dir/file/machine_m0_thm.tex"   ## True
        "dir/file/machine_m0_trans.tex" ## True
        "dir/file/machine_m1.tex" ## True
        "dir/file/machine_m1_co.tex"    ## True
        "dir/file/machine_m1_inv.tex"   ## True
        "dir/file/machine_m1_prog.tex"  ## True
        "dir/file/machine_m1_props.tex" ## True
        "dir/file/machine_m1_saf.tex"   ## True
        "dir/file/machine_m1_thm.tex"   ## True
        "dir/file/machine_m1_trans.tex" ## True
        "dir/file/machine_m2.tex" ## True
        "dir/file/machine_m2_co.tex"    ## True
        "dir/file/machine_m2_inv.tex"   ## True
        "dir/file/machine_m2_prog.tex"  ## True
        "dir/file/machine_m2_props.tex" ## True
        "dir/file/machine_m2_saf.tex"   ## True
        "dir/file/machine_m2_thm.tex"   ## True
        "dir/file/machine_m2_trans.tex" ## True
        "dir/file/machine_m3.tex" ## True
        "dir/file/machine_m3_co.tex"    ## True
        "dir/file/machine_m3_inv.tex"   ## True
        "dir/file/machine_m3_prog.tex"  ## True
        "dir/file/machine_m3_props.tex" ## True
        "dir/file/machine_m3_saf.tex"   ## True
        "dir/file/machine_m3_thm.tex"   ## True
        "dir/file/machine_m3_trans.tex" ## True

case4 :: IO (Either String (Map FilePath Bool))
case4 = runEitherT $ do
    s <- get_system path0
    return $ M.map isJust $ view' files $ execMockFileSystem 
        $ produce_summaries "dir/file.ext" s