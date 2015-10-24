module Statsd.Datastore where

import Control.Concurrent.STM (atomically, newTMVar, putTMVar, takeTMVar, STM, TMVar)

import Statsd.Metrics (Metric)

type DataStore = TMVar [Metric]

newDatastore :: STM DataStore
newDatastore = newTMVar []

newDatastoreIO :: IO DataStore
newDatastoreIO = atomically newDatastore

storeMetrics :: DataStore -> [Metric] -> STM ()
storeMetrics datastore metrics = do
    prevMetrics <- takeTMVar datastore
    putTMVar datastore (prevMetrics ++ metrics)

storeMetricsIO :: DataStore -> [Metric] -> IO ()
storeMetricsIO datastore = atomically . storeMetrics datastore

withDatastoreMetricsIO :: DataStore -> ([Metric] -> ([Metric], [Metric])) -> IO [Metric]
withDatastoreMetricsIO datastore action = atomically $ withDatastoreMetrics datastore action

withDatastoreMetrics :: DataStore -> ([Metric] -> ([Metric], [Metric])) -> STM [Metric]
withDatastoreMetrics datastore action = do
    metrics <- takeTMVar datastore
    let (handledMetrics, unhandledMetrics) = action metrics
    putTMVar datastore unhandledMetrics
    return handledMetrics
