{-# LANGUAGE ExistentialQuantification
        , ImplicitParams, CPP #-} 
module Test.UnitTest 
    ( TestCase(..)
    , run_quickCheck_suite
    , run_quickCheck_suite_with
    , run_test_cases
    , run_test_cases_with
    , test_cases 
    , tempFile, takeLeaves, leafCount
    , selectLeaf, dropLeaves, leaves
    , makeTestSuite, makeTestSuiteOnly
    , testName, TestName
    , stringCase, aCase
    , textCase
    , callStackLineInfo
    , M, UnitTest(..) 
    , IsTestCase(..)
    , logNothing, PrintLog
    , path
    , clear_results
    , allLeaves )
where

    -- Libraries
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.SSem
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Control.Exception.Lens
import Control.Lens hiding ((<.>))
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Precondition

import           Data.Either
import           Data.IORef
import           Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.String.Indentation
import           Data.Text.Lines 
import           Data.Text as Text (unpack,Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as Lazy
import           Data.Tuple
import           Data.Typeable

import GHC.Stack
#if !(MIN_VERSION_base(4,9,0))
import GHC.SrcLoc
#endif

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Prelude

import System.Directory
import System.FilePath
import System.Process
import System.IO
import System.IO.Unsafe

import Test.QuickCheck
import Test.QuickCheck.Report

import Text.Printf.TH

data TestCase = 
      forall a . (Show a, Eq a, Typeable a, NFData a) => Case Text (IO a) a
    | forall a . (Show a, Eq a, Typeable a, NFData a) 
        => CalcCase Text (IO a) (IO a) 
    | TextCase Text (IO Text) Text
    | LineSetCase Text (IO Text) Text
    | Suite CallStack Text [TestCase]
    | WithLineInfo CallStack TestCase
    | QuickCheckProps Text (forall a. (PropName -> Property -> IO (a,Result)) -> IO ([a],Bool))
    | forall test. IsTestCase test => Other test

stringCase :: Pre
           => String
           -> IO String 
           -> String
           -> TestCase
stringCase n test res = WithLineInfo (?loc) $ 
        TextCase (T.pack n) (T.pack <$> test) (T.pack res)

textCase :: Pre
         => Text 
         -> IO Text 
         -> Text
         -> TestCase
textCase n test res = WithLineInfo (?loc) $ TextCase n test res

aCase :: (Pre,Eq a,Show a,Typeable a,NFData a)
      => Text
      -> IO a 
      -> a
      -> TestCase
aCase n test res = WithLineInfo (?loc) $ Case n test res

class Typeable c => IsTestCase c where
    makeCase :: Maybe CallStack -> c -> ReaderT Args IO UnitTest
    nameOf :: Lens' c Text

instance IsTestCase TestCase where
    makeCase _ (WithLineInfo cs t) = makeCase (Just cs) t
    makeCase _ (Suite cs n xs) = Node cs n <$> mapM (makeCase $ Just cs) xs
    makeCase cs (Case x y z) = return UT
                        { name = x
                        , routine = sequenceOf _1 (y,logNothing)
                        , outcome = z
                        , _mcallStack = cs
                        , _displayA = disp
                        , _displayE = disp
                        , _criterion = id
                        }
    makeCase cs (CalcCase x y z) = do 
            r <- lift z
            return UT
                { name = x
                , routine  = (,logNothing) <$> y
                , outcome  = r
                , _mcallStack = cs
                , _displayA = disp
                , _displayE = disp
                , _criterion = id
                }
    makeCase cs (TextCase x y z) = return UT 
                            { name = x
                            , routine = (,logNothing) <$> y
                            , outcome = z
                            , _mcallStack = cs
                            , _displayA = id
                            , _displayE = id
                            , _criterion = id
                            }
    makeCase cs (LineSetCase x y z) = makeCase cs $ textCase x 
                                ((asLines %~ NE.sort) <$> y) 
                                (z & asLines %~ NE.sort)
    makeCase cs (QuickCheckProps n prop) = do
            args <- ask
            let formatResult x = (x & _1.traverse %~ T.pack ,logNothing)
            return UT
                { name = n
                , routine = formatResult <$> prop (quickCheckWithResult' args) 
                , outcome = True
                , _mcallStack = cs
                , _displayA = T.intercalate "\n" . fst
                , _displayE = const ""
                , _criterion = snd
                }
    makeCase cs (Other c) = makeCase cs c
    nameOf f (WithLineInfo x0 c) = WithLineInfo x0 <$> nameOf f c
    nameOf f (Suite x0 n x1) = (\n' -> Suite x0 n' x1) <$> f n
    nameOf f (Case n x0 x1) = (\n' -> Case n' x0 x1) <$> f n
    nameOf f (QuickCheckProps n prop) = (\n' -> QuickCheckProps n' prop) <$> f n
    nameOf f (Other c) = Other <$> nameOf f c
    nameOf f (CalcCase n x0 x1) = (\n' -> CalcCase n' x0 x1) <$> f n
    nameOf f (TextCase n x0 x1) = (\n' -> TextCase n' x0 x1) <$> f n
    nameOf f (LineSetCase n x0 x1) = (\n' -> LineSetCase n' x0 x1) <$> f n

newtype M a = M { runM :: RWST Int [Either (STM [Text]) Text] Int (ReaderT (IORef [ThreadId]) IO) a }
    deriving ( Monad,Functor,Applicative,MonadIO
             , MonadReader Int
             , MonadState Int
             , MonadWriter [Either (STM [Text]) Text])

instance Indentation Int M where
    -- func = 
    margin_string = do
        n <- margin
        return $ T.replicate n "|  "
    _margin _ = id
            
onlyQuickCheck :: TestCase -> Maybe TestCase
onlyQuickCheck (Suite cs n ts) = Suite cs n <$> nonEmpty (mapMaybe onlyQuickCheck ts)
    where
        nonEmpty [] = Nothing
        nonEmpty xs@(_:_) = Just xs
onlyQuickCheck (WithLineInfo cs t) = WithLineInfo cs <$> onlyQuickCheck t
onlyQuickCheck p@(QuickCheckProps _ _) = Just p
onlyQuickCheck _ = Nothing

log_failures :: MVar Bool
log_failures = unsafePerformIO $ newMVar True

failure_number :: MVar Int
failure_number = unsafePerformIO $ newMVar 0

take_failure_number :: M ()
take_failure_number = do
    n <- liftIO $ takeMVar failure_number
    liftIO $ putMVar failure_number $ n+1
    put n

callStackLineInfo :: CallStack -> [Text]
callStackLineInfo cs = reverse $ map f $ filter ((__FILE__ /=) . srcLocFile) $ map snd $ getCallStack cs
    where
        f c = [st|%s:%d:%d|] (srcLocFile c) (srcLocStartLine c) (srcLocStartCol c)


new_failure :: CallStack -> Text -> Text -> Text -> M ()
new_failure cs name actual expected = do
    b <- liftIO $ readMVar log_failures
    if b then do
        n <- get
        liftIO $ withFile ([s|actual-%d.txt|] n) WriteMode $ \h -> do
            T.hPutStrLn h $ "; " <> name
            forM_ (callStackLineInfo cs) $ T.hPutStrLn h . ("; " <>)
            T.hPutStrLn h "; END HEADER"
            T.hPutStrLn h actual
        liftIO $ withFile ([s|expected-%d.txt|] n) WriteMode $ \h -> do
            T.hPutStrLn h $ "; " <> name
            forM_ (callStackLineInfo cs) $ T.hPutStrLn h . ("; " <>)
            T.hPutStrLn h "; END HEADER"
            T.hPutStrLn h expected
    else return ()

test_cases :: Pre => Text -> [TestCase] -> TestCase
test_cases = Suite ?loc

logNothing :: PrintLog
logNothing = const $ const $ const $ const $ return ()

type PrintLog = CallStack -> Text -> Text -> Text -> M ()

data UnitTest = forall a b. (Eq a,NFData b) => UT 
    { name :: Text
    , routine :: IO (b, PrintLog)
    , outcome :: a
    , _mcallStack :: Maybe CallStack
    , _displayA :: b -> Text
    , _displayE :: a -> Text
    , _criterion :: b -> a
    -- , _source :: FilePath
    }
    | Node { _callStack :: CallStack, name :: Text, _children :: [UnitTest] }

-- strip_line_info :: Text -> Text
-- strip_line_info xs = unlines $ map f $ lines xs
--     where
--         f xs = takeWhile (/= '(') xs

clear_results :: IO ()
clear_results = void $ system "rm actual-*.txt expected-*.txt log-*"

run_quickCheck_suite :: Pre => TestCase -> IO Bool
run_quickCheck_suite t = run_quickCheck_suite_with t $ return ()

run_quickCheck_suite_with :: Pre => TestCase -> State Args k -> IO Bool
run_quickCheck_suite_with t = maybe noProps run_test_cases_with (onlyQuickCheck t)
    where
        noProps _ = putStrLn "No QuickCheckProps" >> return True

run_test_cases :: (Pre,IsTestCase testCase) 
               => testCase -> IO Bool
run_test_cases xs = run_test_cases_with xs (return ())

run_test_cases_with :: (Pre,IsTestCase testCase) 
                    => testCase 
                    -> State Args k
                    -> IO Bool
run_test_cases_with xs opts = do
        let args = execState opts stdArgs
        _ <- swapMVar failure_number 0
        c        <- runReaderT (makeCase Nothing xs) args
        ref      <- newIORef []
        (b,_,w)  <- runReaderT (runRWST 
                        (runM $ test_suite_string ?loc c) 0 
                        (assertFalse' "??")) ref
        forM_ w $ \ln -> do
            case ln of
                Right xs -> T.putStrLn xs
                Left xs -> atomically xs >>= mapM_ T.putStrLn
        x <- fmap (uncurry (==)) <$> atomically b
        either throw return x

disp :: (Typeable a, Show a) => a -> Text
disp x = fromMaybe (reindentText $ T.pack $ show x) (cast x <|> fmap T.pack (cast x) <|> fmap Lazy.toStrict (cast x))

putLn :: Text -> M ()
putLn xs = do
        ys <- mk_lines xs
        tell $ map Right ys

test_suite_string :: CallStack
                  -> UnitTest 
                  -> M (STM (Either SomeException (Int,Int)))
test_suite_string cs' ut = do
        case ut of
          (UT title test expected mli dispA dispE cri) -> forkTest $ do
            let cs = fromMaybe cs' mli
            putLn ("+- " <> title)
            r <- liftIO $ catch 
                (Right <$> (liftIO . evaluate . force =<< test)) 
                (\e -> return $ Left $ T.pack $ show (e :: SomeException))
            case r of
                Right (r,printLog) -> 
                    if (cri r == expected)
                    then return (1,1)
                    else do
                        take_failure_number
                        printLog cs title (dispA r) (dispE expected)
                        new_failure cs title (dispA r) (dispE expected)
                        putLn "*** FAILED ***"
                        forM_ (callStackLineInfo cs) $ tell . (:[]) . Right
                        return (0,1) 
                Left m -> do
                    tell [Right $ "   Exception:  \n" <> m]
                    take_failure_number
                    new_failure cs title m (dispE expected)
                    putLn "*** FAILED ***"
                    forM_ (callStackLineInfo cs) $ tell . (:[]) . Right
                    return (0,1)
          Node cs n xs -> do
            putLn ("+- " <> n)
            xs <- indent 1 $ mapM (test_suite_string cs) xs
            forkTest $ do
                xs' <- mergeAll xs
                let xs = map (either (const (0,1)) id) xs' :: [(Int,Int)]
                    x = sum $ map snd xs
                    y = sum $ map fst xs
                putLn ([st|+- [ Success: %d / %d ]|] y x)
                return (y,x)


leaves :: TestCase -> [Text]
leaves (Suite _ _ xs) = concatMap leaves xs
leaves t = [t^.nameOf]


allLeaves :: TestCase -> [TestCase]
allLeaves = allLeaves' ""
    where
        allLeaves' n (Suite _ n' xs) = concatMap (allLeaves' (n <> n' <> "/")) xs
        allLeaves' n t = [t & nameOf %~ (n <>)]

selectLeaf :: Int -> TestCase -> TestCase 
selectLeaf n = takeLeaves (n+1) . dropLeaves n

dropLeaves :: Int -> TestCase -> TestCase
dropLeaves n (Suite cs name xs) = Suite cs name (drop (length ws) xs)
    where
        ys = map leafCount xs
        zs = map sum $ inits ys
        ws = dropWhile (<= n) zs
dropLeaves _ x = x

takeLeaves :: Int -> TestCase -> TestCase
takeLeaves n (Suite cs name xs) = Suite cs name (take (length ws) xs)
    where
        ys = map leafCount xs
        zs = map sum $ inits ys
        ws = takeWhile (<= n) zs
takeLeaves _ x = x

leafCount :: TestCase -> Int
leafCount (Suite _ _ xs) = sum $ map leafCount xs
leafCount _ = 1

capabilities :: SSem
capabilities = unsafePerformIO $ new 32

newtype Handled = Handled SomeException 
    deriving (Show, Exception)

_Handled :: Prism' SomeException Handled
_Handled = prism' toException fromException

forkTest :: M (Int,Int) -> M (STM (Either SomeException (Int,Int)))
forkTest cmd = do
    result <- liftIO $ newEmptyTMVarIO
    output <- liftIO $ newEmptyTMVarIO
    r <- ask
    liftIO $ wait capabilities
    ref <- M $ lift ask
    t <- liftIO $ do
        ref <- newIORef []
        let handler e = do
                putStrLn "failed"
                print e
                atomically $ do 
                    putTMVar result $ Left e
                    putTMVar output $ [T.pack $ show e]
        forkIO $ do
            finally (handling id handler $ do
                (x,_,w) <- runReaderT (runRWST (runM cmd) r (-1)) ref
                atomically $ putTMVar result (Right x)
                xs <- forM w $ \ln -> do
                    either 
                        atomically 
                        (return . (:[])) 
                        ln
                atomically $ putTMVar output $ concat xs
                )
                (signal capabilities)
    liftIO $ modifyIORef ref (t:)
    tell [Left $ readTMVar output]
    return $ readTMVar result

mergeAll :: [STM a] -> M [a]
mergeAll xs = liftIO $ do
    forM xs atomically

tempFile_num :: MVar Int
tempFile_num = unsafePerformIO $ newMVar 0

tempFile :: FilePath -> IO FilePath
tempFile path = do
    n <- takeMVar tempFile_num
    putMVar tempFile_num (n+1)
    -- path <- canonicalizePath path
    let path' = dropExtension path ++ "-" ++ show n <.> takeExtension path
    --     finalize = do
    --         b <- doesFileExist path'
    --         when b $
    --             removeFile path'
    -- mkWeakPtr path' (Just finalize)
    return path'

data TestName = TestName Text CallStack

data TestCaseGen = PropCaseGen Name Name | CaseGen Name Name Name

caseGenName :: TestCaseGen -> Name
caseGenName (PropCaseGen n _) = n
caseGenName (CaseGen n _ _)   = n

matchTestCase :: Int -> Q (Either Int TestCaseGen)
matchTestCase n = fmap (maybe (Left n) Right) 
            $ runMaybeT $ msum
        [ liftA2 PropCaseGen (valueName namei) (valueName propi)
        , CaseGen <$> valueName namei <*> valueName casei <*> valueName resulti ]
    where
        valueName = MaybeT . lookupValueName
        namei = "name" ++ show n
        casei = "case" ++ show n
        resulti = "result" ++ show n
        propi = "prop" ++ show n

genTestCase :: TestCaseGen -> ExpQ
genTestCase (CaseGen n c r) = [e| Case (fooNameOf $(varE n)) $(varE c) $(varE r) |]
genTestCase (PropCaseGen n prop) = [e| QuickCheckProps (fooNameOf $(varE n)) $(varE prop) |]        

testName :: Pre => Text -> TestName
testName str = TestName str ?loc

fooNameOf :: TestName -> Text
fooNameOf (TestName str _)   = str

fooCallStack :: TestName -> CallStack
fooCallStack (TestName _ cs) = cs

makeTestSuiteOnly :: Text -> [TestCaseGen] -> ExpQ
makeTestSuiteOnly title ts = do
        let loci i = [e| fooCallStack $(varE $ caseGenName i) |]
            cases = [ [e| WithLineInfo $(loci i) $(genTestCase i) |] | i <- ts ]
            titleE = stringE (unpack title)
        [e| test_cases $titleE $(listE cases) |]

makeTestSuite :: Text -> ExpQ
makeTestSuite title = do
    let names n' = [ "name" ++ n' 
                   , "case" ++ n' 
                   , "result" ++ n' 
                   , "prop" ++ n' ]
        f :: Int -> Q Bool
        f n = do
            let n' = show n
            any isJust <$> mapM lookupValueName (names n')
    xs <- concat <$> sequence
        [ takeWhileM f [0..0]
        , takeWhileM f [1..] ]
    (es,ts) <- partitionEithers <$> mapM matchTestCase xs
    if null es then do
        makeTestSuiteOnly title ts
    else do
        mapM_ (reportError . [s|missing component for test case # %d|]) es
        [e| undefined |]

path :: QuasiQuoter
path = QuasiQuoter 
    { quoteExp = \xs -> do
            -- dir <- runIO $ getEnv "UNITB_ROOT"
            dir <- runIO getCurrentDirectory
            -- loc <- location
            let xs' = dir </> xs
                -- dir = takeDirectory (loc_filename loc)
            -- xs' <- runIO $ canonicalizePath xs 
            exists <- runIO (doesFileExist xs')  
            -- fail $ "file " ++ show xs ++ " does not exist in " ++ show dir
            -- fail $ loc_filename loc ++ "\n" ++ loc_module loc
            when (not exists) $ fail $ "file " ++ show xs ++ " does not exist in " ++ show dir
            stringE xs'
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }
