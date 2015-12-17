{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent (myThreadId, threadDelay)
import Data.Conduit.Attoparsec (ParseError, PositionRange)
import qualified Data.Conduit.List as CL
import Data.Conduit
import Data.Conduit.Network
import Data.Streaming.Network
import GHC.Conc (labelThread)
import System.Log.Logger

import Statsd.Config
import Statsd.Datastore
import Statsd.Flush
import Statsd.Metrics
import Statsd.Parser
import Statsd.Utils


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
startListener datastore options = do
    let portNum = port options
    noticeM "statsd.listener" $ "Listening on port " ++ show portNum
    runTCPServer (serverSettings portNum "*") $ serverApp datastore

-- | The TCP Server app which ingests metrics
serverApp :: Datastore -> AppData -> IO ()
serverApp datastore appData = do
    infoM "statsd.listener" $ "Client connected on " ++ show (appSockAddr appData)
    appSource appData $= addCleanup (const $ infoM "statsd.listener" $ "Client disconnected from " ++ show (appSockAddr appData))
        individualMetricConduit $$ metricHandler datastore

-- | Start the datastore handler
startHandler :: Datastore -> Options -> IO ()
startHandler datastore options = do
    debugM "statsd.handler" $ "Running datastore handler (delaying: " ++ show (flushInterval options) ++ ")"
    handledMetrics <- withDatastoreMetricsIO datastore flushMetrics
    infoM "statsd.handler" $ "Handled " ++ show (length $ datastoreToList handledMetrics) ++ " metrics."
    threadDelay $ flushInterval options
    startHandler datastore options

-- | Handler to store metrics in the datastore
metricHandler :: Datastore -> Sink (Either ParseError (PositionRange, Metric)) IO ()
metricHandler datastore = CL.mapM_ handle
    -- TODO report error back to client?
    where handle (Left error) = errorM "statsd.listener" $ "Error parsing metric: " ++ show error
          handle (Right (_, metric)) = do
            debugM "statsd.listener" $ "Recieved metric: " ++ show metric
            storeMetricIO datastore metric
