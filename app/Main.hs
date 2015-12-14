import Control.Concurrent (myThreadId, threadDelay)
import Data.Conduit.Attoparsec (ParseError, PositionRange)
import qualified Data.Conduit.List as CL
import Data.Conduit.Network (sourceSocket)
import Data.Conduit (($$), ($=), Sink)
import Data.Streaming.Network (acceptSafe)
import GHC.Conc (labelThread)
import Network (listenOn, withSocketsDo, Socket, PortID (PortNumber))
import System.Log.Logger

import Statsd.Config
import Statsd.Datastore
import Statsd.Flush
import Statsd.Metrics
import Statsd.Parser
import Statsd.Utils


metricHandler :: Datastore -> Sink (Either ParseError (PositionRange, Metric)) IO ()
metricHandler datastore = CL.mapM_ handle
    -- TODO report error back to client?
    where handle (Left error) = errorM "statsd.listener" $ "Error parsing metric: " ++ show error
          handle (Right (_, metric)) = do
            debugM "statsd.listener" $ "Recieved metric: " ++ show metric
            storeMetricIO datastore metric

sockHandler :: Datastore -> Socket -> IO ()
sockHandler datastore sock = do
    -- TODO thread per connection, cant handle multiple connections?
    -- TODO close socket?
    (socket, addr) <- acceptSafe sock
    infoM "statsd.listener" $ "Client connected on " ++ show addr
    sourceSocket socket $= individualMetricConduit $$ metricHandler datastore
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
    l <- async' "metricListener" $ startListener datastore options
    h <- async' "metricHandler" $ startHandler datastore options
    debugM "statsd" "Started all threads"
    waitForAll [l, h]

-- | Start the network listener, and pass it onto the socket handler
startListener :: Datastore -> Options -> IO ()
startListener datastore options = withSocketsDo $ do
    let portNum = port options
    sock <- listenOn $ PortNumber $ fromInteger portNum
    noticeM "statsd.listener" $ "Listening on port " ++ show portNum
    sockHandler datastore sock

-- | Start the datastore handler
startHandler :: Datastore -> Options -> IO ()
startHandler datastore options = do
    debugM "statsd.handler" $ "Running datastore handler (delaying: " ++ show (flushInterval options) ++ ")"
    handledMetrics <- withDatastoreMetricsIO datastore flushMetrics
    infoM "statsd.handler" $ "Handled " ++ show (length $ datastoreToList handledMetrics) ++ " metrics."
    threadDelay $ flushInterval options
    startHandler datastore options
