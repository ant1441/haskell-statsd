module Statsd.Flush where

import Statsd.Datastore

flushMetrics :: Datastore' -> ([a], Datastore')
flushMetrics metrics = ([], error "no new datastore")
