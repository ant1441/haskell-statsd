module Statsd.Flush where

import Statsd.Datastore

flushMetrics :: Datastore' -> (Datastore', Datastore')
flushMetrics metrics = (metrics, newDatastore)
