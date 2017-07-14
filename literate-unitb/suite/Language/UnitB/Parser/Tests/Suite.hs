module Language.UnitB.Parser.Tests.Suite 
    ( module Language.UnitB.Parser.Tests.Suite 
    , module Control.Monad.Trans.Either 
    , module Logic.UnitTest
    , module Language.UnitB
    , Error(..) )
where

    -- Modules
import Language.UnitB.Parser as Doc

import Logic.Expr as E
import Logic.Proof
import Logic.UnitTest

import Language.UnitB.PO
import Language.UnitB hiding (proof_obligation,raw_proof_obligation)
import qualified Language.UnitB as UB

import Z3.Z3

    -- Libraries
import Control.Arrow hiding (right,left)
import Control.Concurrent
import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Control.Precondition ((!))

import Control.Monad.State
import Control.Monad.Trans.Either

import Data.Either.Combinators
import Data.List as L hiding (lookup)
import qualified Data.List.NonEmpty as NE
import Data.HashMap.Lazy as M hiding (lookup,(!))
import Data.HashMap.Lazy.Extras as M
import Data.Monoid
import           Data.Text (Text,pack)
import qualified Data.Text as T
import Data.Time

import Prelude hiding (lookup)
import PseudoMacros

import System.Directory
import System.IO.Unsafe
import System.Process

import Text.Printf.TH

import Utilities.Syntactic

type POResult = (Text,HashMap Label Sequent)
type POS sid a = Either [Error] (HashMap sid (a, HashMap Label Sequent))

hide_error_path :: Bool
hide_error_path = True

pos :: MVar (HashMap (NonEmpty FilePath) 
                 ((POS MachineId Machine,POS String Theory), HashMap FilePath UTCTime))
pos = unsafePerformIO $ newMVar M.empty

list_file_obligations' :: FilePath -> IO (POS MachineId Machine,POS String Theory)
list_file_obligations' path = many_file_obligations' $ pure path

many_file_obligations' :: NonEmpty FilePath -> IO (POS MachineId Machine,POS String Theory)
many_file_obligations' files = do
        --  modifyMVar is important for exception safety
        --  if this procedure crashes without restoring the
        --  state of pos, every other thread attempting any 
        --  call on this module is going to with a mysterious
        --  MVar exception
    b <- liftIO $ mapM doesFileExist files
    if and b then do
        modifyMVar pos $ \m -> do
            files <- mapM canonicalizePath files
            t <- fmap M.fromList $ forM (NE.toList files) $ \file -> 
                (file,) <$> getModificationTime file
            sys <- parse_system' files
            let cmd :: Monad m => (b -> m c) -> b -> m (b,c)
                cmd f = runKleisli (Kleisli return &&& Kleisli f)
                -- ms :: Either 
                ms = M.map (id &&& UB.proof_obligation).view' machines <$> sys 
                    -- >>= traverseWithKey (cmd PO.proof_obligation)
                ts = view' theories <$> sys >>= traverse (cmd theory_po)
                m' = M.insert (NE.sort files) ((ms,ts),t) m
            return (m',(ms,ts))
    else do
        let fs = L.map snd $ NE.filter (not.fst) $ NE.zip b files
            errs = Left [Error ([st|The following files do not exist: %s|] $ intercalate "," fs) $ LI "" 1 1]
        return (errs,errs)

verifyFiles :: NonEmpty FilePath 
            -> Int -> IO POResult
verifyFiles = verifyFilesWith (return ())

verify :: FilePath -> Int -> IO POResult
verify = verifyWith (return ())

verifyOnly :: FilePath -> Text -> Int -> IO POResult
verifyOnly fp lbl = verifyFilesOnlyWith (return ()) (label lbl ==) (pure fp)

verifyWith :: State Sequent a
           -> FilePath 
           -> Int 
           -> IO POResult
verifyWith cmd fp = verifyFilesWith cmd $ pure fp

verifyFilesWith :: State Sequent a
                -> NonEmpty FilePath
                -> Int 
                -> IO POResult
verifyFilesWith opt = verifyFilesOnlyWith opt (const True)

verifyFilesOnlyWith :: State Sequent a
                    -> (Label -> Bool)
                    -> NonEmpty FilePath
                    -> Int 
                    -> IO POResult
verifyFilesOnlyWith opt keep files i = makeReport' empty $ do
    b <- liftIO $ mapM doesFileExist files
    if and b then do
        ms <- EitherT $ fst <$> many_file_obligations' files
        if i < size ms then do
            let (m,pos) = snd (i `elemAt` ms)
            r <- lift $ trying id (str_verify_machine_with (\lbl s -> guard (keep lbl) >> Just (execState opt s)) m)
            case r of
                Right (s,_,_) -> return (s, pos)
                Left e -> return (pack $ show (e :: SomeException),pos)
        else return ([st|accessing %dth refinement out of %d|] i (size ms),empty)
    else do
        let fs = L.map snd $ NE.filter (not.fst) $ NE.zip b files
        return ([st|The following files do not exist: %s|] $ intercalate "," fs,empty)

all_proof_obligations' :: FilePath -> EitherT Text IO [HashMap Label Text]
all_proof_obligations' path = do
    exists <- liftIO $ doesFileExist path
    if exists then do
        xs <- bimapEitherT (pack . show) id
            $ EitherT $ fst <$> list_file_obligations' path
        let pos = M.elems $ M.map snd xs
            cmd = L.map (M.map z3_code) pos
        return cmd
    else left $ [st|file does not exist: %s|] path

all_proof_obligations :: FilePath -> IO (Either Text [HashMap Label Text])
all_proof_obligations = runEitherT . all_proof_obligations'

withLI :: Pre => Either Text a -> Either [Error] a
withLI = mapLeft $ errorTrace [] ?loc

withLI' :: (Pre,Monad m) => EitherT Text m a -> EitherT [Error] m a
withLI' (EitherT cmd) = EitherT $ withLI <$> cmd

raw_proof_obligation :: FilePath -> Text -> Int -> IO Text
raw_proof_obligation path lbl i = makeReport $ do
    exists <- liftIO $ doesFileExist path
    if exists then do
        ms <- EitherT $ Doc.parse_machine path
        m  <- Language.UnitB.Parser.Tests.Suite.lookup i ms
        po <- hoistEither $ withLI $ lookupSequent 
                (label lbl) 
                (UB.raw_proof_obligation m)
        let cmd = z3_code po
        return $ [st|; %s\n%s; %s\n|] lbl cmd lbl
    else return $ [st|file does not exist: %s|] path

stripAnnotation :: Expr -> Expr
stripAnnotation e = E.rewrite stripAnnotation $ f e
    where
        strip = set annotation [] 
        f (FunApp fun xs) = FunApp (strip fun) xs
        f e = e

proof_obligation_stripped :: FilePath -> Text -> Int -> IO Text
proof_obligation_stripped = proof_obligation_with stripAnnotation

proof_obligation :: FilePath -> Text -> Int -> IO Text
proof_obligation = proof_obligation_with id

lookupSequent :: Label -> HashMap Label Sequent -> Either Text Sequent
lookupSequent lbl pos = case pos^?ix lbl of
                Just po -> 
                    Right po
                Nothing ->
                    Left $ [st|invalid label: %s\n%s|] (prettyText lbl) $ 
                        T.unlines $ L.map prettyText $ keys pos

lookupSequent' :: Monad m 
               => Label -> HashMap Label Sequent 
               -> EitherT Text m Sequent
lookupSequent' lbl m = hoistEither $ lookupSequent lbl m

sequent :: FilePath -> Text 
        -> Int -> IO (Either Text Sequent)
sequent path lbl i = runEitherT $ sequent' path lbl i

sequent' :: FilePath -> Text 
         -> Int -> EitherT Text IO Sequent
sequent' path lbl i = do
        xs <- EitherT $ (mapLeft show_err . fst) 
            <$> list_file_obligations' path
        if i < size xs then do
            let pos = snd $ snd (i `elemAt` xs)
            lookupSequent' (label lbl) pos
        else
            left $ [st|accessing %dth refinement out of %d|] i (size xs)   

proof_obligation_with :: (Expr -> Expr) 
                      -> FilePath -> Text 
                      -> Int -> IO Text
proof_obligation_with f path lbl i = either id disp <$> sequent path lbl i
    where
        disp po = [st|; %s\n%s; %s\n|] lbl (cmd po) lbl
        cmd po = z3_code 
                  $ po & nameless %~ L.map f
                       & named %~ M.map f
                       & goal %~ f

find_errors :: FilePath -> IO Text 
find_errors path = do
    p <- canonicalizePath path
    exists <- doesFileExist path
    if exists then do
        m <- fst <$> list_file_obligations' path
        let hide
                | hide_error_path = T.replace (pack p <> ":") "error "
                | otherwise       = id
        return $ either 
            (hide . T.intercalate "\n" . L.map ((<> "\n") . report))
            (const $ "no errors")
            m
    else return $ [st|file does not exist: %s|] path

parse_machine :: FilePath -> Int -> IO (Either [Error] Machine)
parse_machine path n = runEitherT $ parse_machine' path n

parse_machine' :: FilePath -> Int -> EitherT [Error] IO Machine
parse_machine' fn i = lookup i =<< parse' fn

parse :: FilePath -> IO (Either [Error] [Machine])
parse path = do
    p <- getCurrentDirectory
    exists <- doesFileExist path
    let mapError = traverse.traverseLineInfo.filename %~ drop (length p)
        f = elems . M.map fst
    if exists then
        (mapBoth mapError f . fst) <$> list_file_obligations' path
    else 
        return $ Left [Error ([st|'%s' does not exist|] path) $ LI "" 1 1]

parse' :: FilePath -> EitherT [Error] IO [Machine]
parse' = EitherT . parse

verify_thy :: FilePath -> String -> IO POResult
verify_thy path name = makeReport' empty $ do
        exists <- liftIO $ doesFileExist path
        if exists then do
            r <- EitherT $ snd <$> list_file_obligations' path
            let pos = snd $ r ! name
            res <- lift $ trying id (verify_all pos)
            case res of
                Right res -> return (T.unlines $ L.map (\(k,r) -> success r <> prettyText k) $ toAscList res, pos)
                Left e -> return (pack $ show e,pos)
        else return ([st|file does not exist: %s|] path,empty)
    where
        success True  = "  o  "
        success False = " xxx "

get_system :: FilePath -> EitherT Text IO System
get_system path = do
    exists <- liftIO $ doesFileExist path
    if exists then do
        EitherT $ either (Left . show_err) Right 
            <$> parse_system path
    else left $ [st|file does not exist: %s|] path

get_system' :: FilePath -> EitherT [Error] IO System
get_system' path = do
    exists <- liftIO $ doesFileExist path
    let li = LI "get_system'" 1 1
    if exists then do
        EitherT $ parse_system path
    else left $ [Error ([st|file does not exist: %s|] path) li]

lookup :: (Pre,Monad m,Ixed f,Show (Index f)) => Index f -> f -> EitherT [Error] m (IxValue f)
lookup k m = maybe (left $ errorTrace [$__FILE__] ?loc (pack $ show k)) return $ m^?ix k

edit :: FilePath -> IO ()
edit str = do
    _ <- readProcess "edit" [] str
    return ()
