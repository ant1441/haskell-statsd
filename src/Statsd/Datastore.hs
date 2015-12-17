{-# LANGUAGE TemplateHaskell #-}
module Statsd.Datastore where

import Control.Concurrent.STM (atomically, newTMVar, putTMVar, takeTMVar, STM, TMVar)
import Control.Lens

import Statsd.Metrics

data Datastore' = Datastore' { _gauges :: Gauges,
                               _counters :: Counters,
                               _timers :: Timers,
                               _histograms :: Histograms,
                               _meters :: Meters }
    deriving (Show, Eq)
makeLenses ''Datastore'

type Datastore = TMVar Datastore'

newDatastore :: Datastore'
newDatastore = Datastore' [] [] [] [] []

newDatastoreSTM :: STM Datastore
newDatastoreSTM = newTMVar newDatastore

newDatastoreIO :: IO Datastore
newDatastoreIO = atomically newDatastoreSTM

storeMetric, storeGauge, storeCounter, storeTimer, storeHistogram, storeMeter :: Metric -> Datastore' -> Datastore'
storeMetric metric = func metric
    where func = case metricType metric of
            Gauge       -> storeGauge
            Counter     -> storeCounter
            Timer       -> storeTimer
            Histogram   -> storeHistogram
            Meter       -> storeMeter

storeGauge m = over gauges (m:)
storeCounter m = over counters (m:)
storeTimer m = over timers (m:)
storeHistogram m = over histograms (m:)
storeMeter m = over meters (m:)

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

toList :: Datastore' -> [Metric]
toList datastore = concatMap (datastore ^.) [gauges, counters, timers, histograms, meters]
