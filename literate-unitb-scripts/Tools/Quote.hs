module Tools.Quote where

import Control.Monad

import System.Process

import           Pipes
import qualified Pipes.Prelude as P
import           Pipes.Safe
import qualified Pipes.Safe.Prelude as P

escape :: String -> String
escape xs = concatMap f xs
    where
        f '\t' = "\\t"
        f '\\' = "\\\\"
        f '\"' = "\\\""
        f x    = [x]

-- allBut :: Int -> [a] -> [a]
-- allBut k xs = take (n - k) xs
--     where
--         n = length xs

quote :: MonadSafe m
      => FilePath 
      -> Producer String m ()
quote fn = markLast (P.readFile fn) >-> quoteLn

initP :: Monad m
      => Pipe (Maybe a) (Maybe a) m ()
initP = maybe (return ()) aux =<< await
    where
        aux x = do
            y <- await
            case y of
                Just y' -> do
                    yield (Just x)
                    aux y'
                Nothing -> yield Nothing

accumWhile :: Monad m
           => (a -> Bool) 
           -> Pipe a b m [a]
accumWhile p = do
    x <- await
    if p x 
        then (x:) <$> accumWhile p
        else return []

markLast :: Monad m
         => Proxy a' a () b m r
         -> Proxy a' a () (Maybe b) m r
markLast p = do
        x <- p >-> P.map Just
        yield Nothing
        return x

drawAll :: Monad m => Pipe (Maybe a) a m ()
drawAll = maybe (return ()) (const drawAll <=< yield) =<< await

quoteLn' :: MonadSafe m
         => Pipe (Maybe String) String m ()
quoteLn' = do
        h <- P.concat >-> header
        finally
            (do drawAll >-> do
                    x <- await
                    yield $ "    [ \"" ++ x ++ "\""
                    forever $ do
                        ln <- await
                        yield $ "    , \"" ++ ln ++ "\""
                yield "    ]")
            (liftIO $ rawSystem "subl" [drop 2 $ head h])
    where
        header = accumWhile ("; END HEADER" /=)

quoteLn :: MonadSafe m => Pipe (Maybe String) String m ()
quoteLn = P.drop 1 >-> P.map (fmap escape) >-> quoteLn'
        -- lns <- (map escape . drop 1 . lines) 
        --     `liftM` readFile fn
        -- let lns' = drop 1 $ dropWhile ("; END HEADER" /=) lns
        --             -- remove header
        -- yield $ "    [ \"" ++ (lns' ! 0) ++ "\""
        -- forM_ (allBut 1 $ drop 1 lns') $ \ln -> do
        --     yield $ "    , \"" ++ ln ++ "\""
        -- yield "    ]"
        -- --system $ "runhaskell find_case.hs " ++ fn
        -- rawSystem "subl" [drop 2 $ head lns]
        -- return ()

-- main :: IO ()
-- main = do
--         fn <- getArgs
--         case fn of
--             [fn] -> runEffect $ runSafeP 
--                 $ quote fn >-> copyToClipboard
--             _ -> putStrLn "usage: quote file"
