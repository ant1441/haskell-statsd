module Statsd.Datastore where

import Control.Concurrent.STM (atomically, newTMVar, putTMVar, takeTMVar, STM, TMVar)

import Statsd.Metrics

type Datastore' = (Gauges, Counters, Timers, Histograms, Meters)
type Datastore = TMVar Datastore'

newDatastore :: Datastore'
newDatastore = ([], [], [], [], [])

newDatastoreSTM :: STM Datastore
newDatastoreSTM = newTMVar newDatastore

newDatastoreIO :: IO Datastore
newDatastoreIO = atomically newDatastoreSTM

storeMetrics :: [Metric] -> Datastore' -> Datastore'
storeMetrics [] datastore = datastore
storeMetrics (m:ms) (gauges, counters, timers, histograms, meters) =
    case metricType m of
        Gauge       -> storeMetrics ms (m:gauges, counters, timers, histograms, meters)
        Counter     -> storeMetrics ms (gauges, m:counters, timers, histograms, meters)
        Timer       -> storeMetrics ms (gauges, counters, m:timers, histograms, meters)
        Histogram   -> storeMetrics ms (gauges, counters, timers, m:histograms, meters)
        Meter       -> storeMetrics ms (gauges, counters, timers, histograms, m:meters)

storeMetricsSTM :: Datastore -> [Metric] -> STM ()
storeMetricsSTM datastore metrics = do
    prevMetrics <- takeTMVar datastore
    let newMetrics = storeMetrics metrics prevMetrics
    putTMVar datastore newMetrics

storeMetricsIO :: Datastore -> [Metric] -> IO ()
storeMetricsIO datastore = atomically . storeMetricsSTM datastore

withDatastoreMetricsIO :: Datastore -> (Datastore' -> ([a], Datastore')) -> IO [a]
withDatastoreMetricsIO datastore action = atomically $ withDatastoreMetrics datastore action

withDatastoreMetrics :: Datastore -> (Datastore' -> ([a], Datastore')) -> STM [a]
withDatastoreMetrics datastore action = do
    metrics <- takeTMVar datastore
    -- Perform the action on the metrics, returning a tuple of handled, unhandled metrics
    let (handledMetrics, unhandledMetrics) = action metrics
    putTMVar datastore unhandledMetrics
    return handledMetrics
