{-# LANGUAGE OverloadedStrings #-}
module Statsd.Test.Datastore where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Concurrent.STM (atomically, takeTMVar, STM)
import Control.Lens

import Statsd.Metrics
import Statsd.Datastore

datastoreSpec :: Spec
datastoreSpec = do
    describe "Pure datastore functions" $ do
        describe "storeMetric" $ do
            it "returns a filled datastore when given a single gauge" $
                storeMetric singleGauge newDatastore `shouldBe` filledDatastoreG
            it "returns a filled datastore when given a single counter" $
                storeMetric singleCounter newDatastore `shouldBe` filledDatastoreC
            it "returns a filled datastore when given a single timer" $
                storeMetric singleTimer newDatastore `shouldBe` filledDatastoreT
            it "returns a filled datastore when given a single histogram" $
                storeMetric singleHistogram newDatastore `shouldBe` filledDatastoreH
            it "returns a filled datastore when given a single meter" $
                storeMetric singleMeter newDatastore `shouldBe` filledDatastoreM
            it "returns a datastore with a single metric when given the same counter twice" $
                toList (storeMetric singleCounter (storeMetric singleCounter newDatastore)) `shouldBe` [Metric Counter "counter.name" 2 (Just 1.0)]


-- Vars

singleGauge :: Metric
singleGauge = Metric Gauge "gauge.name" 1 Nothing

singleCounter :: Metric
singleCounter = Metric Counter "counter.name" 1 (Just 1.0)

singleTimer :: Metric
singleTimer = Metric Timer "timer.name" 1 Nothing

singleHistogram :: Metric
singleHistogram = Metric Histogram "hist.name" 1 Nothing

singleMeter :: Metric
singleMeter = Metric Meter "meter.name" 1 Nothing

allMetrics :: [Metric]
allMetrics = [singleGauge, singleCounter, singleTimer, singleHistogram, singleMeter]

filledDatastoreG :: Datastore'
filledDatastoreG = newDatastore & gauges .~ [singleGauge]

filledDatastoreC :: Datastore'
filledDatastoreC = newDatastore & counters .~ [singleCounter]

filledDatastoreT :: Datastore'
filledDatastoreT = newDatastore & timers .~ [singleTimer]

filledDatastoreH :: Datastore'
filledDatastoreH = newDatastore & histograms .~ [singleHistogram]

filledDatastoreM :: Datastore'
filledDatastoreM = newDatastore & meters .~ [singleMeter]

filledDatastoreA :: Datastore'
filledDatastoreA = Datastore' [singleGauge] [singleCounter] [singleTimer] [singleHistogram] [singleMeter]
