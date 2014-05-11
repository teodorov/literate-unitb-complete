
import Control.Monad
import Control.Monad.Fix

import Data.List

import Interactive.Config

import System.Environment

import Tools.Source
    
import Utilities.Directory

main :: IO ()
main = do
        xs <- getArgs
        case xs of
            x:_ -> do
                xs <- (drop 2 . head . lines) `liftM` readFile x
                sources <- visit "." [".hs",".lhs"]
                ys <- concat `liftM` forM sources (\f -> do
                    zs <- (zip [1..] . lines) `liftM` readFile f
                    let p (_,ln) = xs `isInfixOf` ln
                    return $ zip (map fst $ filter p zs) $ repeat f)
                forM_ ys $ \(n,fn) -> do
                    edit n fn
                    fix $ \rec -> do
                        xs <- getLine
                        if xs /= ":q" then do
                            goto_definition fn xs
                            rec
                        else return ()
            [] -> putStrLn "Usage: find_case error_file"
