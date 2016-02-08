module Utilities.QuickCheckReport where

import Control.Monad

import Data.IORef

import Test.QuickCheck

import Text.Printf

isSuccess :: Result -> Bool
isSuccess (Success {  }) = True
isSuccess _ = False

test_report :: Testable a
            => ((a -> IO Result) -> IO b) -> IO Bool
test_report tests = do 
    success <- newIORef (0 :: Int)
    total   <- newIORef (0 :: Int)
    let inc r = do
            when (isSuccess r) 
                $ modifyIORef success (+1)
            modifyIORef total (+1)
            return r
    (tests $ (>>= inc) . quickCheckWithResult stdArgs {chatty = False})
    x <- readIORef success
    y <- readIORef total
    printf "success: %d / %d\n[ %s ]\n" 
        x y
        (if x == y then "passed" else "failed")
    return $ x == y