import Test.Hspec

import Statsd.Test.Datastore
import Statsd.Test.Metrics
import Statsd.Test.Parser
import Statsd.Test.Utils

main :: IO ()
main = hspec $ do
        describe "Statsd.Datastore" datastoreSpec
        describe "Statsd.Metrics" metricSpec
        describe "Statsd.Parser" parserSpec
        describe "Statsd.Utils" utilsSpec
