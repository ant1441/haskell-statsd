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
import Statsd.Flush
import Statsd.Metrics
import Statsd.Parser
import Statsd.Utils

reportError :: Handle -> String -> IO ()
reportError handle str = do
    hPutStr handle str
    errorM "statsd.listener" str

connectionHandler :: Datastore -> Handle -> HostName -> PortNumber -> IO ()
connectionHandler datastore handle hostname portNum = do
    debugM "statsd.listener" $ "Connection from " ++ hostname ++ ":" ++ show portNum
    eitherMetrics <- metricProcessor handle
    case eitherMetrics of
        Left error -> reportError handle error
        Right metrics -> storeMetricsIO datastore metrics
    hClose handle
    debugM "statsd.listener" $ "Disconnected " ++ hostname ++ ":" ++ show portNum

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
    debugM "statsd" "Started all threads"
    mapM_ wait [l, h]

-- | Start the network listener, and pass it onto the socket handler
startListner :: Datastore -> Options -> IO ()
startListner datastore options = withSocketsDo $ do
    let portNum = port options

    t <- myThreadId
    labelThread t "metricListener"

    sock <- listenOn $ PortNumber $ fromInteger portNum
    noticeM "statsd.listener" $ "Listening on port " ++ show portNum
    sockHandler datastore sock

-- | Start the datastore handler
startHandler :: Datastore -> Options -> IO ()
startHandler datastore options = do

    t <- myThreadId
    labelThread t "metricHandler"

    debugM "statsd.handler" "Running datastore handler"
    handledMetrics <- withDatastoreMetricsIO datastore flushMetrics
    debugM "statsd.handler" $ "Handled " ++ show (length handledMetrics) ++ " metrics."
    threadDelay $ flushInterval options
    startHandler datastore options
