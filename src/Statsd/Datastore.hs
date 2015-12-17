{-# LANGUAGE TemplateHaskell #-}
module Statsd.Datastore where

import Control.Concurrent.STM (atomically, newTMVar, putTMVar, takeTMVar, STM, TMVar)
import Control.Lens
import qualified Data.Map.Strict as Map

import Statsd.Metrics

-- * Collection types
type Gauges     = Map.Map Name Metric
type Counters   = Map.Map Name Metric
type Timers     = Map.Map Name Metric
type Histograms = Map.Map Name Metric
type Meters     = Map.Map Name Metric

data Datastore' = Datastore' { _gauges :: Gauges,
                               _counters :: Counters,
                               _timers :: Timers,
                               _histograms :: Histograms,
                               _meters :: Meters }
    deriving (Show, Eq)
makeLenses ''Datastore'

type Datastore = TMVar Datastore'

newDatastore :: Datastore'
newDatastore = Datastore' Map.empty Map.empty Map.empty Map.empty Map.empty

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

storeGauge m     = over gauges     $ Map.insertWith (error "storeGauge TODO") (name m) m
storeCounter m   = over counters   $ Map.insertWith addCounter (name m) m
storeTimer m     = over timers     $ Map.insertWith (error "storeTimer TODO") (name m) m
storeHistogram m = over histograms $ Map.insertWith (error "storeHistogram TODO") (name m) m
storeMeter m     = over meters     $ Map.insertWith (error "storeMeter TODO") (name m) m

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

datastoreMap :: Datastore' -> (Map.Map Name Metric -> a) -> [a]
datastoreMap datastore f = map (f . (datastore ^.)) [gauges, counters, timers, histograms, meters]

length :: Datastore' -> Int
length datastore = sum $ datastoreMap datastore Map.size

-- * Map helpers
metricSingleton :: Metric -> Map.Map Name Metric
metricSingleton metric = Map.singleton (name metric) metric
