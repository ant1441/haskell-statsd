import Control.Concurrent.Async (async, wait)
import Control.Concurrent (forkIO, myThreadId, threadDelay)
import GHC.Conc (labelThread)
import qualified Data.ByteString as S
import Data.Either (lefts, rights)
import Network (accept, listenOn, withSocketsDo, HostName, Socket, PortID (..), PortNumber)
import System.IO (hClose, hPutStr, hSetBinaryMode, hSetBuffering, Handle, BufferMode(NoBuffering))

import Statsd.Config
import Statsd.Datastore
import Statsd.Metrics
import Statsd.Parser

reportError :: Handle -> String -> IO ()
reportError = hPutStr

connectionHandler :: DataStore -> Handle -> HostName -> PortNumber -> IO ()
connectionHandler datastore handle hostname portNum = do
    --putStrLn $ "Connection from " ++ hostname ++ "[" ++ show portNum ++ "]"
    eitherMetrics <- metricProcessor handle
    case eitherMetrics of
        Left error -> reportError handle error
        Right metrics -> storeMetricsIO datastore metrics
    hClose handle

sockHandler :: DataStore -> Socket -> IO ()
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

start :: DataStore -> Options -> IO ()
start datastore options = do
    l <- async $ startListner datastore options
    h <- async $ startHandler datastore options
    mapM_ wait [l, h]

startListner :: DataStore -> Options -> IO ()
startListner datastore options = withSocketsDo $ do
    let portNum = port options
    sock <- listenOn $ PortNumber $ fromInteger portNum
    putStrLn $ "Listening on port " ++ show portNum
    sockHandler datastore sock

startHandler :: DataStore -> Options -> IO ()
startHandler datastore options = do
    putStrLn "Running datastore handler"
    handledMetrics <- withDatastoreMetricsIO datastore (\m -> do
        (m, []))
    putStrLn $ "Handled " ++ show (length handledMetrics) ++ " metrics."
    threadDelay $ flushInterval options
    startHandler datastore options
