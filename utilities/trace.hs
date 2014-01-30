{-# LANGUAGE BangPatterns #-}
module Utilities.Trace where

import Control.Concurrent
import Control.Monad.IO.Class

import Data.Set

import qualified Debug.Trace as DT

import System.IO.Unsafe

import Utilities.Format

trace_switch = unsafePerformIO (newMVar empty)

is_tracing_on = do
        sw  <- readMVar trace_switch
        tid <- myThreadId
        return $ tid `member` sw

turn_tracing_on = modifyMVar_ trace_switch $ \sw -> do
        tid <- myThreadId
        return $ insert tid sw

turn_tracing_off = modifyMVar_ trace_switch $ \sw -> do
        tid <- myThreadId
        return $ delete tid sw

trace xs x = unsafePerformIO $ do
        tid <- myThreadId
        b <- is_tracing_on
        if b then
            return (DT.trace (format "({0}) {1}" tid xs) x)
        else
            return x

traceM :: Monad m => String -> m ()
traceM xs = trace xs (return ())

traceIO xs = do
        tid <- liftIO $ myThreadId
        b <- liftIO $ is_tracing_on
        if b then
            liftIO $ DT.traceIO (format "({0}) {1}" tid xs)
        else
            return ()

traceBlock :: Monad m => String -> m a -> m a
traceBlock xs cmd = do
        traceM $ "begin " ++ xs
        r <- cmd
        traceM $ "end " ++ xs
        return r

traceBlockIO :: MonadIO m => String -> m a -> m a
traceBlockIO xs cmd = do
        traceIO $ "begin " ++ xs
        r <- cmd
        traceIO $ "end " ++ xs
        return r

with_tracing x = unsafePerformIO $ do
        b <- is_tracing_on
        if b 
            then return () 
            else turn_tracing_on
        let !r = x
        if b
            then return ()
            else turn_tracing_off
        return r

with_tracingIO cmd = do
        b <- liftIO $ is_tracing_on
        if b 
            then return () 
            else liftIO $ turn_tracing_on
        r <- cmd
        if b
            then return ()
            else liftIO $ turn_tracing_off
        return r

