import Control.Concurrent.Async (async, wait)
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import GHC.Conc (labelThread)
import qualified Data.ByteString as S
import Data.Either (lefts, rights)
import Network (accept, listenOn, withSocketsDo, HostName, Socket, PortID (..), PortNumber)
import System.IO (hClose, hPutStr, hSetBinaryMode, hSetBuffering, Handle, BufferMode(NoBuffering))
import System.Log.Logger

import Statsd.Config
import Statsd.Datastore
import Statsd.Metrics
import Statsd.Parser
import Statsd.Utils

reportError :: Handle -> String -> IO ()
reportError = hPutStr

connectionHandler :: Datastore -> Handle -> HostName -> PortNumber -> IO ()
connectionHandler datastore handle hostname portNum = do
    debugM "statsd" $ "Connection from " ++ hostname ++ "[" ++ show portNum ++ "]"
    eitherMetrics <- metricProcessor handle
    case eitherMetrics of
        Left error -> reportError handle error
        Right metrics -> storeMetricsIO datastore metrics
    hClose handle

sockHandler :: Datastore -> Socket -> IO ()
sockHandler datastore sock = do
    (handle, hostname, portNum) <- accept sock
    hSetBuffering handle NoBuffering
    hSetBinaryMode handle True
    _ <- forkIO $ connectionHandler datastore handle hostname portNum
    sockHandler datastore sock

main = do
    t <- myThreadId
    labelThread t "main"
    database <- newDatastoreIO
    parseArgs $ start database

-- | Start all the tasks, and wait for their completion (forever)
start :: Datastore -> Options -> IO ()
start datastore options = do
    configureLogging options
    l <- async $ startListner datastore options
    h <- async $ startHandler datastore options
    mapM_ wait [l, h]

-- | Start the network listener, and pass it onto the socket handler
startListner :: Datastore -> Options -> IO ()
startListner datastore options = withSocketsDo $ do
    let portNum = port options
    sock <- listenOn $ PortNumber $ fromInteger portNum
    noticeM "statsd" $ "Listening on port " ++ show portNum
    sockHandler datastore sock

-- | Start the datastore handler
startHandler :: Datastore -> Options -> IO ()
startHandler datastore options = do
    debugM "statsd" "Running datastore handler"
    handledMetrics <- withDatastoreMetricsIO datastore (\m -> do
        (m, []))
    debugM "statsd" $ "Handled " ++ show (length handledMetrics) ++ " metrics."
    threadDelay $ flushInterval options
    startHandler datastore options
