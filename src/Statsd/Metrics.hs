-- |
-- Module      :  $Header$
-- Copyright   :  (c) Adam Hodgen
-- License     :  <license>
--
-- Stability   :  experimental
-- Portability :  unknown
--
-- Metric types for StatsD.

module Statsd.Metrics (Name, Value, SampleRate, Metric(..)) where

import Data.ByteString.Char8 (ByteString)

type Name       = ByteString
type Value      = Double
type SampleRate = Double

-- | StatsD metrics, will be stored with the @value@ stored against the @name@.
-- They are generally of the form @\<metric name\>:\<value\>|c[|\<sample rate\>]@ (see 'Statsd.Parser').
-- Different metrics will aggregate and transform the @value@ in specific ways.
-- Details taken from <https://github.com/b/statsd_spec>
data Metric = Gauge Name Value              -- <metric name>:<value>|g
              -- ^ Instantaneous measurement of a value.
              -- __Note__ This differs from the 'Counter' by being calculated by the client.
            | Counter Name Value SampleRate
              -- ^ A counter is a 'Gauge' calculated by the server.
              -- When a client sends a 'Counter', it increments or decrements the stored value.
              -- An optional sample rate can be sent, given as a decimal value of the
              -- number of samples per event count.
            | Timer Name Value              -- <metric name>:<value>|ms
              -- ^ A timer is the number of milliseconds elapsed between a start and end time.
            | Histogram Name Value          -- <metric name>:<value>|h
              -- ^ A histogram is a measure of the distribution of timer values over time, calculated at the server.
              -- __Note__ Currently an alias for 'Timer'.
            | Meter Name Value              -- <metric name>:<value>|m
              -- ^ A meter is a measure of the rate of events.
              -- May be thought of as an increment only timer.
            deriving (Eq, Show)

type Storage = [Metric]
