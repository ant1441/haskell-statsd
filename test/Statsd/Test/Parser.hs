{-# LANGUAGE OverloadedStrings #-}
module Statsd.Test.Parser (metricSpec) where

import Test.Hspec
import Test.Hspec.Attoparsec

import Data.ByteString.Char8 (ByteString)

import Statsd.Parser

metricSpec :: Spec
metricSpec = do
    describe "metric parser - success cases" $ do
        it "succesfully parses a guage" $
            ("value:1|g\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "g" Nothing]
        it "succesfully parses a timer" $
            ("value:1|ms\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "ms" Nothing]
        it "succesfully parses a histogram" $
            ("value:1|h\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "h" Nothing]
        it "succesfully parses a meter" $
            ("value:1|m\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "m" Nothing]
        it "succesfully parses a counter" $
            ("value:1|c\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "c" Nothing]
        it "succesfully parses a counter with a sample rate" $
            ("value:1|c|@0.2\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "c" (Just 0.2)]
        it "should handle windows and unix newlines" $ do
            metricParser `shouldSucceedOn` ("value:1|g\r\n" :: ByteString)
            metricParser `shouldSucceedOn` ("value:1|g\n" :: ByteString)
            metricParser `shouldSucceedOn` ("value:1|c|@2\r\n" :: ByteString)
            metricParser `shouldSucceedOn` ("value:1|c|@2\n" :: ByteString)
        it "should not accept just a carriage return" $
            metricParser `shouldFailOn` ("value:1|g\r" :: ByteString)
        it "should be forgiving of a missing newline" $ do
            metricParser `shouldSucceedOn` ("value:1|g" :: ByteString)
            metricParser `shouldSucceedOn` ("value:1|c" :: ByteString)
            metricParser `shouldSucceedOn` ("value:1|c|@2" :: ByteString)
    describe "metric parser - multi metric success cases" $ do
        it "succesfully parses two guages" $
            ("value:1|g\nanotherval:1|g\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "g" Nothing, MetricMsg "anotherval" 1 "g" Nothing]
        it "succesfully parses two counters" $
            ("value:1|c\nanotherval:1|c\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "c" Nothing, MetricMsg "anotherval" 1 "c" Nothing]
        it "succesfully parses two counters, the first with a sample rate" $
            ("value:1|c|@3\nanotherval:1|c\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "c" (Just 3), MetricMsg "anotherval" 1 "c" Nothing]
        it "succesfully parses two counters, the second with a sample rate" $
            ("value:1|c\nanotherval:1|c|@3\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "c" Nothing, MetricMsg "anotherval" 1 "c" (Just 3)]
        it "succesfully parses two counters with a sample rate" $
            ("value:1|c|@3\nanotherval:1|c|@5\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "c" (Just 3), MetricMsg "anotherval" 1 "c" (Just 5)]
        it "succesfully parses a guage and a timer" $
            ("value:1|g\nvalue:1|ms\n" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "g" Nothing, MetricMsg "value" 1 "ms" Nothing]
        it "should handle windows and unix newlines" $ do
            metricParser `shouldSucceedOn` ("value:1|g\r\nanotherval:1|g\n" :: ByteString)
            metricParser `shouldSucceedOn` ("value:1|g\nanotherval:1|g\r\n" :: ByteString)
            metricParser `shouldSucceedOn` ("value:1|g\r\nanotherval:1|g\r\n" :: ByteString)
        it "should be forgiving of a final missing newline" $
            ("value:1|g\nvalue:1|ms" :: ByteString) ~> metricParser `shouldParse` [MetricMsg "value" 1 "g" Nothing, MetricMsg "value" 1 "ms" Nothing]
    describe "metric parser - failing cases" $ do
        it "fails on an invalid metric type" $
            metricParser `shouldFailOn` ("value:1|z\n" :: ByteString)
        it "fails on an invalid seperators" $ do
            metricParser `shouldFailOn` ("value-1|g\n" :: ByteString)
            metricParser `shouldFailOn` ("value:1-g\n" :: ByteString)
