{-# LANGUAGE BangPatterns #-}
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

storeMetric :: Metric -> Datastore' -> Datastore'
storeMetric m (gauges, counters, timers, histograms, meters) =
    case metricType m of
        Gauge       -> (m:gauges, counters, timers, histograms, meters)
        Counter     -> (gauges, m:counters, timers, histograms, meters)
        Timer       -> (gauges, counters, m:timers, histograms, meters)
        Histogram   -> (gauges, counters, timers, m:histograms, meters)
        Meter       -> (gauges, counters, timers, histograms, m:meters)

storeMetricSTM :: Datastore -> Metric -> STM ()
storeMetricSTM datastore metrics = do
    prevMetrics <- takeTMVar datastore
    let newMetrics = storeMetric metrics prevMetrics
    putTMVar datastore newMetrics

storeMetricsIO :: Datastore -> [Metric] -> IO ()
storeMetricsIO = mapM_ . storeMetricIO

storeMetricIO :: Datastore -> Metric -> IO ()
storeMetricIO datastore  = atomically . storeMetricSTM datastore

withDatastoreMetricsIO :: Datastore -> (Datastore' -> (Datastore', Datastore')) -> IO Datastore'
withDatastoreMetricsIO datastore action = atomically $ withDatastoreMetrics datastore action

withDatastoreMetrics :: Datastore -> (Datastore' -> (Datastore', Datastore')) -> STM Datastore'
withDatastoreMetrics datastore action = do
    metrics <- takeTMVar datastore
    -- Perform the action on the metrics, returning a tuple of handled, unhandled metrics
    -- TODO: Should I make this strict to ensure flushing happens right now?
    let (handledMetrics, unhandledMetrics) = action metrics
    -- BUG: if action returns (_, error "_"), this hangs, why?
    putTMVar datastore unhandledMetrics
    return handledMetrics

datastoreToList :: Datastore' -> [Metric]
datastoreToList (g, c, t, h, m) = g ++ c ++ t ++ h ++ m
