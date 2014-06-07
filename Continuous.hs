#!/usr/bin/env runhaskell
{-# LANGUAGE FlexibleContexts #-}
module Main where

    -- Modules
import Interactive.Pipeline
import Interactive.Config

import Document.Document

import Logic.Expr

import UnitB.AST
import UnitB.PO

import Z3.Z3

    -- Libraries
import Control.Monad
import Control.Exception
import Control.Monad.State
import Control.Monad.Trans.Either 

import Data.Time
import Data.Typeable

import qualified Data.ByteString as BS
import qualified Data.Map as M
import           Data.Map as M 
    ( Map
    , empty 
    , insert
    , toList
    )
import qualified Data.Serialize as Ser

import System.Console.GetOpt
import System.Directory
import System.Environment
import System.Locale

import Text.Printf

import Utilities.Syntactic

with_po_map :: StateT Params IO a -> Params -> IO ()
with_po_map act param = do
        let fn = path param ++ ".state'"
        b <- doesFileExist fn
        param <- if b && not (no_verif param) && not (reset param) then do
            xs <- BS.readFile fn
            either 
                (\_ -> return param) 
                (\po -> return param { pos = po }) 
                $ Ser.decode xs
        else return param
        void $ execStateT act param

dump_pos :: MonadIO m => StateT Params m ()
dump_pos = do
        d <- gets no_dump
        v <- gets no_verif
        if not d && not v then do
            p    <- gets pos
            file <- gets path   
            let fn = file ++ ".state'"
    --        liftIO $ Ser.dump_pos file p
            liftIO $ forM_ (toList p) $ \(n,p) -> do
                dump (show n) (M.map snd p)
            liftIO $ BS.writeFile fn $ Ser.encode p
        else return ()

monitor :: (Monad m, Eq s) 
        => m s -> m () 
        -> m () -> m ()
monitor measure delay act = do
    t <- measure
    runStateT (
        forever (do
            t0 <- get
            t1 <- lift measure
            if t0 == t1
                then return ()
                else do 
                    put t1
                    lift act
            lift delay)) t
    return ()

data Params = Params
        { path :: FilePath
        , verbose :: Bool
        , continuous :: Bool
        , no_dump :: Bool
        , no_verif :: Bool
        , reset :: Bool
        , pos :: Map Label (Map Label (Bool,Sequent))
        }

check_one :: (MonadIO m, MonadState Params m) 
          => Machine -> m (Int,String)
check_one m = do
        param <- get
        let p = M.lookup (_name m) $ pos param
        let po = maybe empty id p
        (po,s,n)    <- liftIO $ verify_changes m po
        put (param { pos = insert (_name m) po $ pos param })
        return (n,"> machine " ++ show (_name m) ++ ":\n" ++ s)

check_theory :: (MonadIO m, MonadState Params m) 
             => (String,Theory) -> m (Int,String)
check_theory (name,th) = do
        param <- get
        let old_po = maybe empty id $ M.lookup (label name) $ pos param
            po = either undefined id $ theory_po th
            new_po = po `M.difference` old_po
            ch_po  = po `M.intersection` old_po
            ch     = M.filterWithKey (\k x -> snd (old_po M.! k) /= x) ch_po
            all_po = new_po `M.union` ch
--            handle :: ErrorCall -> IO (Map Label Bool)
                -- revise: do not return empty
            handle (SomeException e) = do
                    putStrLn "> error" 
                    print (typeOf e) 
                    print e
                    return $ M.map (const False) all_po 
        res    <- liftIO $ catch 
                    (verify_all all_po)
                    handle
        let p_r = M.mapWithKey f po
            f k x = maybe (old_po M.! k) (\b -> (b,x)) $ M.lookup k res
        put param { pos = insert (label name) p_r $ pos param }
        let s = unlines $ map (\(k,r) -> success r ++ show k) $ toList res
        return (M.size res,"> theory " ++ show (label name) ++ ":\n" ++ s)
    where
        success True  = "  o  "
        success False = " xxx "

clear :: (MonadIO m, MonadState Params m) 
      => m ()
clear = do
    param <- get
    if continuous param 
        then liftIO $ putStr $ take 40 $ cycle "\n"
        else return ()

check_file :: (MonadIO m, MonadState Params m) 
           => m ()
check_file = do
        param <- get
        let { p ln = verbose param || take 4 ln /= "  o " }
        r <- liftIO $ runEitherT $ do
            s <- EitherT $ parse_system $ path param
            lift $ produce_summaries s
            return (M.elems $ machines s, M.toList $ theories s)
        case r of
            Right (ms,ts) -> do
                if no_verif param then return ()
                else do
                    xs <- forM ms check_one
                    clear
                    forM_ xs $ \(n,xs) -> liftIO $ do
                        forM_ (filter p $ lines xs) 
                            putStrLn
                        putStrLn $ "Redid " ++ show n ++ " proofs"
                    ys <- forM ts check_theory
                    forM_ ys $ \(n,xs) -> liftIO $ do
                        forM_ (filter p $ lines xs) 
                            putStrLn
                        putStrLn $ "Redid " ++ show n ++ " proofs"
            Left xs -> do
                clear
                forM_ xs (\(Error x (LI _ i j)) -> liftIO $ 
                    printf "error (%d,%d): %s\n" i j x)
        liftIO $ putStrLn ""
        tz <- liftIO (getCurrentTimeZone)
        t  <- liftIO (getCurrentTime :: IO UTCTime)
        liftIO $ putStrLn $ formatTime defaultTimeLocale "Time: %H:%M:%S" $ utcToLocalTime tz t

data Option = Verbose | Continuous | NoDump | NoVerif | Reset
    deriving Eq

options :: [OptDescr Option]
options =
    [ Option ['V'] ["verbose"] (NoArg Verbose) 
            "display all successful proof obligations" 
    , Option ['c'] ["continuous"] (NoArg Continuous) 
            "monitor input files and reverify when they change"
    , Option ['d'] ["nodump"] (NoArg NoDump) 
            "do not serialize the proof obligations after verification"
    , Option ['v'] ["noverif"] (NoArg NoVerif) 
            "do not generate and discharge the proof obligations"
    , Option ['r'] ["reset"] (NoArg Reset)
            "discard the previous results of verification"
    ]

main :: IO ()
main = do
        rawargs <- getArgs
        let (opts,args,_) = getOpt Permute options rawargs
        case args of
            [xs] -> do
                b1 <- doesFileExist xs
                b2 <- z3_installed
                if b1 && b2 then do
                    let { param = Params 
                            { path = xs
                            , pos = empty
                            , no_dump    = NoDump `elem` opts
                            , no_verif   = NoVerif `elem` opts
                            , verbose    = Verbose `elem` opts
                            , continuous = Continuous `elem` opts
                            , reset      = Reset `elem` opts
                            } }
                    if continuous param then do
                        run_pipeline xs
                        return ()
                    else
                        with_po_map (do
                            check_file
                            dump_pos) param
                    -- with_po_map (do
                        -- check_file
                        -- dump_pos
                        -- if continuous param 
                        -- then do
                            -- monitor
                                -- (liftIO $ getModificationTime xs)
                                -- (liftIO $ threadDelay 1000000)
                                -- (do check_file
                                    -- dump_pos)
                        -- else return ()) param
                    return ()
                else if not b2 then
                    putStrLn ("a 'z3' executable hasn't been found in the path ")
                else
                    putStrLn ("'" ++ xs ++ "' is not a valid file")
            _ -> putStrLn $ usageInfo "usage: continuous file [options]" options