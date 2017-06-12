module Tools.Clipboard where

import           Pipes
import           Pipes.Safe
import qualified Pipes.Prelude as P
import System.IO
import System.Process


copyToClipboard :: Consumer String (SafeT IO) () 
copyToClipboard = do
    bracket
        (do (stdin,_,_,pid) <- runInteractiveProcess "pbcopy" [] Nothing Nothing
            return (stdin,pid))
        (\(stdin,pid) -> do
            liftIO $ do
                hClose stdin
                waitForProcess pid)
        (P.toHandle . fst)

    -- void . readProcess "pbcopy" []

