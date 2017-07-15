module Logic.UnitTest where

    -- Modules
import Logic.Expr hiding ( name )
import Logic.Proof

import Z3.Z3

    -- Libraries
import Control.Applicative
import Control.Exception
import Control.Lens hiding ((<.>))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Precondition

import           Data.List
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Tuple
import           Data.Typeable

import GHC.Stack

import Prelude

import           Pipes as P
import           Pipes.Safe as P (runSafeT)
import qualified Pipes.Prelude as P

import System.IO

import Test.UnitTest

import Text.Printf.TH

data POCase = POCase Text (IO (Text, M.Map Label Sequent)) Output

poCase :: Pre
       => Text 
       -> IO (Text, M.Map Label Sequent) 
       -> Output
       -> TestCase
poCase n test res = WithLineInfo (?loc) $ Other $ POCase n test res

onlyPoCases :: TestCase -> Maybe TestCase
onlyPoCases (Suite cs n ts) = Suite cs n <$> nonEmpty (mapMaybe onlyPoCases ts)
    where
        nonEmpty [] = Nothing
        nonEmpty xs@(_:_) = Just xs
onlyPoCases (WithLineInfo cs t) = WithLineInfo cs <$> onlyPoCases t
onlyPoCases (Other p) = Other <$> (cast p :: Maybe POCase)
onlyPoCases _ = Nothing

run_poTestSuite :: Pre => TestCase -> IO Bool
run_poTestSuite t = maybe noProps run_test_cases (onlyPoCases t)
    where
        noProps = putStrLn "No QuickCheckProps" >> return True

instance IsTestCase POCase where
    makeCase cs (POCase n test res)     = do
            let cmd = catch (test & mapped._2 %~ print_po) handler
                handler exc = do
                    putStrLn "*** EXCEPTION ***"
                    T.putStrLn n
                    print exc
                    return (pack $ show (exc :: SomeException), logNothing)
            return UT
                { name = n
                , routine = cmd
                , outcome = res
                , _mcallStack = cs
                , _displayA = P.each . T.lines
                , _displayE = id
                , _criterion = P.each . T.lines
                , _matches = checkEq
                }
    nameOf f (POCase n test res) = (\n' -> POCase n' test res) <$> f n

print_po :: M.Map Label Sequent 
         -> CallStack 
         -> Text 
         -> Output
         -> Output
         -> M ()
print_po pos cs name actual expected = do
    n <- get
    liftIO $ do
        let f :: Output -> IO (M.Map Text Bool)
            f xs = M.map (== "  o  ") . M.fromList . map (swap . T.splitAt 5) <$> runSafeT (P.toListM xs)
        ma <- f actual
        me <- f expected
        let mr = M.keys $ M.filter not $ M.unionWith (==) (me `M.intersection` ma) ma
        forM_ (zip [0 :: Int ..] mr) $ \(i,po) -> do
            if label po `M.member` pos then do
                withFile ([s|po-%d-%d.z3|] n i) WriteMode $ \h -> do
                    T.hPutStrLn h $ "; " <> name
                    T.hPutStrLn h $ "; " <> po
                    T.hPutStrLn h $ "; " <> if not $ ma ! po 
                                            then  "does {not discharge} automatically"
                                            else  "{discharges} automatically"
                    forM_ (callStackLineInfo cs) $ T.hPutStrLn h . ("; " <>)
                    T.hPutStrLn h $ T.unlines $ z3_code (pos ! label po) : ["; " <> po]
            else return ()
