{-# LANGUAGE OverloadedStrings #-}
module Statsd.Test.Metrics where

import Control.Exception (evaluate)
import Test.Hspec


import Statsd.Metrics

metricSpec :: Spec
metricSpec =
    describe "addCounter" $ do
        it "Adds two Counters" $
            addCounter aCounter aCounter `shouldBe` Metric Counter "a" 20 (Just 1.0)
        it "Adds two Counters without a sample rate" $
            addCounter bCounter bCounter `shouldBe` Metric Counter "b" 5 (Just 1.0)
        it "errrors when you give counters with different names" $
            evaluate (addCounter aCounter bCounter) `shouldThrow` errorCall "Cannot merge two counters with different names ('a', 'b')"
        it "errrors when you give something not a counter" $
            evaluate (addCounter aMeter bCounter) `shouldThrow` errorCall "addCounter call on non Counter Metric"




aCounter = Metric Counter "a" 10 (Just 1.0)
bCounter = Metric Counter "b" 10 (Just 4.0)
aMeter = Metric Meter "a" 1 Nothing
