{-# OPTIONS_HADDOCK ignore-exports #-}
-- |
-- Module      :  $Header$
-- Copyright   :  (c) Adam Hodgen
-- License     :  <license>
--
-- Stability   :  experimental
-- Portability :  unknown
--
-- Metric types for StatsD.

module Statsd.Metrics where
--module Statsd.Metrics (Metric(Metric), MetricType(..), Gauges, Counters, Timers, Histograms, Meters) where

import Data.ByteString.Char8 (ByteString)

type Name       = ByteString
type Value      = Double
type SampleRate = Maybe Double

-- | StatsD metrics, will be stored with the @value@ stored against the @name@.
-- They are generally of the form @\<metric name\>:\<value\>|c[|\<sample rate\>]@ (see 'Statsd.Parser').
-- Different metrics will aggregate and transform the @value@ in specific ways.
-- Details taken from <https://github.com/b/statsd_spec>
data Metric = Metric { metricType :: MetricType,
                       name       :: Name,
                       value      :: Value,
                       extraData  :: SampleRate }
    deriving (Eq, Show)

data MetricType =
    Gauge           -- <metric name>:<value>|g
                    -- ^ Instantaneous measurement of a value.
                    -- __Note__ This differs from the 'Counter' by being calculated by the client.
    | Counter       -- <metric name>:<value>|c
                    -- ^ A counter is a 'Gauge' calculated by the server.
                    -- When a client sends a 'Counter', it increments or decrements the stored value.
                    -- An optional sample rate can be sent, given as a decimal value of the
                    -- number of samples per event count.
    | Timer         -- <metric name>:<value>|ms
                    -- ^ A timer is the number of milliseconds elapsed between a start and end time.
    | Histogram     -- <metric name>:<value>|h
                    -- ^ A histogram is a measure of the distribution of timer values over time, calculated at the server.
                    -- __Note__ Currently an alias for 'Timer'.
    | Meter         -- <metric name>:<value>|m
                    -- ^ A meter is a measure of the rate of events.
                    -- May be thought of as an increment only timer.
    deriving (Eq, Show, Enum)

-- * Helpers
type Gauges = [Metric]
type Counters = [Metric]
type Timers = [Metric]
type Histograms = [Metric]
type Meters = [Metric]

-- | Helper function to get the sample for a Counter
sampleRate :: Metric -> SampleRate
sampleRate (Metric Counter _ _ e) = e
sampleRate _ = Nothing

-- * Functions to clear a list of each Metric

-- TODO:
-- Look at break, span (Prelude), partition (Data.List)

-- | Clear the counters
clearCounter :: Bool -> Counters -> (Counters, Counters)
clearCounter False counters = (counters, [])
clearCounter True counters = ([], [])

-- | Clear the timers
clearTimers :: Bool -> Timers -> (Timers, Timers)
clearTimers False timers = (timers, [])
clearTimers True timers = ([], [])

-- | Clear the sets
{- TYPE?
clearSets :: Bool -> undefined -> (undefined, undefined)
clearSets False sets = (sets, [])
clearSets True sets = ([], [])
-}

-- | Clear the guages
clearGauges :: Bool -> Gauges -> (Gauges, Gauges)
clearGauges False gauges = (gauges, [])
clearGauges True gauges = ([], [])
