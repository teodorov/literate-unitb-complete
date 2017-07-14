{-# LANGUAGE OverloadedStrings #-}
module Code.Test where

import Code.Synthesis hiding (M)

import Logic.Expr.Const (var)

import Language.UnitB hiding (safety)
import Language.UnitB.Expr
import Language.UnitB.Parser

    -- Libraries
import Control.Precondition

import Data.List as L
import           Data.Text (Text,pack)
import qualified Data.Text as T

import System.IO.Unsafe

import Test.UnitTest

test_case :: TestCase
test_case = test

test :: TestCase
test = test_cases
            "code generation in the cube example"
            [ (textCase "test3: code for the {procedure + loop}" case3 result3) 
            ]


result0 :: Text
result0 = T.unlines
        [ "data State = State"
        , "    { v_a :: Int" 
        , "    , v_b :: Int" 
        , "    , v_c :: Int"
        , "    , v_f :: M.Map (Int) (Int)" 
        , "    , v_n :: Int }" ]

input :: Either Text RawMachineAST
input = unsafePerformIO $ fmap (view' syntax.raw) <$> parse path0

path0 :: FilePath
path0 = [path|Tests/cubes-t8.tex|]

result3 :: Text
result3 = T.unlines
        [ "find_cubes c_N = do"
        , "        execState proc s'"
        , "    where"
        , "        s' = State"
        , "             { v_b = 1"
        , "             , v_c = 6" 
        , "             , v_n = 0"
        , "             , v_a = 0"
        , "             , v_f = M.empty"
        , "             }" 
        , "        proc ="
        , "               fix $ \\proc' -> do"
        , "                 (State { .. }) <- get"
        , "                 if (not (v_n < c_N)) then return ()"
        , "                 else do"
        , "                   s@(State { .. }) <- get"
        , "                   if (v_n < c_N) then do"
        , "                     let s' = s"
        , "                             { v_n = (v_n + 1)"
        , "                             , v_a = (v_a + v_b)"
        , "                             , v_b = (v_b + v_c)"
        , "                             , v_c = (v_c + 6)" 
        , "                             , v_f = (M.insert v_n v_a v_f)"
        , "                             }" 
        , "                     put s'"
        , "                   else"
        , "                     put s" 
        , "                   proc'" 
        ]

case3 :: IO Text
case3 = do let x = do
                m <- raw <$> input
                run $ machine_code "find_cubes" m $ n `zeq` bigN
           return $ either id id x    
    where
        (n)      = fromRight' $ fst $ var "n" int
        (bigN)   = fromRight' $ fst $ var "N" int
     
parse :: FilePath -> IO (Either Text Machine)
parse path = do
    r <- parse_machine path
    case r of
        Right [m] -> do
            return $ Right m
        Right _ -> return $ Left "wrong number of machines"
        Left x  -> return $ Left $ T.unlines $ L.map (pack . show) x

