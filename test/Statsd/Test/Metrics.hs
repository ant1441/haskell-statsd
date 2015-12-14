{-# LANGUAGE OverloadedStrings #-}
module Statsd.Test.Metrics where

import Test.Hspec

import Statsd.Metrics

metricSpec :: Spec
metricSpec =
    describe "Metric clearing functions" $ do
        describe "when configured off" $ do
            it "clearCounter returns an empty list when given one" $
                clearCounter False [] `shouldBe` ([], [])
            it "clearCounter returns the given metrics untouched" $
                clearCounter False singleCounter `shouldBe` (singleCounter, [])

            it "clearTimers returns an empty list when given one" $
                clearTimers False [] `shouldBe` ([], [])
            it "clearTimers returns the given metrics untouched" $
                clearCounter False singleTimer `shouldBe` (singleTimer, [])

{-
            it "clearSets returns an empty list when given one" $
                clearSets False [] `shouldBe` ([], [])
            it "clearSets returns the given metrics untouched" $
                clearCounter False singleSet `shouldBe` (singleSet, [])
-}

            it "clearGauges returns an empty list when given one" $
                clearGauges False [] `shouldBe` ([], [])
            it "clearGauges returns the given metrics untouched" $
                clearCounter False singleGauge `shouldBe` (singleGauge, [])

        describe "when configured on" $ do
            describe "clearCounter" $ do
                it "returns an empty list when given one" $
                    clearCounter True [] `shouldBe` ([], [])
                it "doesn't return a pair of empty list when given some metrics" $
                    clearCounter True singleCounter `shouldNotBe` ([], [])
            describe "clearTimers" $ do
                it "clearTimers returns an empty list when given one" $
                    clearTimers True [] `shouldBe` ([], [])
                it "doesn't return a pair of empty list when given some metrics" $
                    clearTimers True singleTimer `shouldNotBe` ([], [])
{-
            describe "clearSets" $ do
                it "returns an empty list when given one (configured on)" $
                    clearSets True [] `shouldBe` ([], [])
                it "doesn't return a pair of empty list when given some metrics" $
                    clearSets True singleSet `shouldNotBe` ([], [])
-}
            describe "clearGauges" $ do
                it "returns an empty list when given one (configured on)" $
                    clearGauges True [] `shouldBe` ([], [])
                it "doesn't return a pair of empty list when given some metrics" $
                    clearGauges True singleGauge `shouldNotBe` ([], [])

-- Vars

singleGauge :: Gauges
singleGauge = [Metric Gauge "gauge.name" 1 Nothing]

singleCounter :: Counters
singleCounter = [Metric Counter "counter.name" 1 (Just 1.0)]

singleTimer :: Timers
singleTimer = [Metric Timer "timer.name" 1 Nothing]

singleHistogram :: Histograms
singleHistogram = [Metric Histogram "hist.name" 1 Nothing]

singleMeter :: Meters
singleMeter = [Metric Meter "meter.name" 1 Nothing]

allMetrics :: [Metric]
allMetrics = singleGauge ++ singleCounter ++ singleTimer ++ singleHistogram ++ singleMeter
