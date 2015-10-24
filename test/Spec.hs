import Test.Hspec
import Statsd.Test.Parser
import Statsd.Test.Utils

main :: IO ()
main = hspec $ do
        describe "Statsd.Parser" metricSpec
        describe "Statsd.Utils" utilsSpec
