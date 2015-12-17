{-# LANGUAGE OverloadedStrings #-}
module Statsd.Test.Parser (parserSpec) where

import Test.Hspec
import Test.Hspec.Attoparsec

import Control.Applicative
import Control.Exception
import Control.Monad.Writer
import Control.Monad.Catch.Pure
import Data.Attoparsec.Combinator (endOfInput)
import Data.ByteString.Char8 (ByteString)
import Data.Conduit
import Data.Either
import qualified Data.Conduit.List as DCL
import Data.Void

import Statsd.Parser
import Statsd.Metrics

fullParser = metricParser <* endOfInput

parserSpec :: Spec
parserSpec = do
    describe "metric parser - success cases" $ do
        it "succesfully parses a gauge" $
            ("value:1|g" :: ByteString) ~> fullParser `shouldParse` [Metric Gauge "value" 1 Nothing]
        it "succesfully parses a timer" $
            ("value:1|ms" :: ByteString) ~> fullParser `shouldParse` [Metric Timer "value" 1 Nothing]
        it "succesfully parses a histogram" $
            ("value:1|h" :: ByteString) ~> fullParser `shouldParse` [Metric Histogram "value" 1 Nothing]
        it "succesfully parses a meter" $
            ("value:1|m" :: ByteString) ~> fullParser `shouldParse` [Metric Meter "value" 1 Nothing]
        it "succesfully parses a counter" $
            ("value:1|c" :: ByteString) ~> fullParser `shouldParse` [Metric Counter "value" 1 Nothing]
        it "succesfully parses a counter with a sample rate" $
            ("value:1|c|@0.2" :: ByteString) ~> fullParser `shouldParse` [Metric Counter "value" 1 (Just 0.2)]
    describe "metric parser - multi metric success cases" $ do
        it "succesfully parses two gauges" $
            ("value:1|g\nanotherval:1|g" :: ByteString) ~> fullParser `shouldParse` [Metric Gauge "value" 1 Nothing, Metric Gauge "anotherval" 1 Nothing]
        it "succesfully parses two counters" $
            ("value:1|c\nanotherval:1|c" :: ByteString) ~> fullParser `shouldParse` [Metric Counter "value" 1 Nothing, Metric Counter "anotherval" 1 Nothing]
        it "succesfully parses two counters, the first with a sample rate" $
            ("value:1|c|@3\nanotherval:1|c" :: ByteString) ~> fullParser `shouldParse` [Metric Counter "value" 1 (Just 3), Metric Counter "anotherval" 1 Nothing]
        it "succesfully parses two counters, the second with a sample rate" $
            ("value:1|c\nanotherval:1|c|@3" :: ByteString) ~> fullParser `shouldParse` [Metric Counter "value" 1 Nothing, Metric Counter "anotherval" 1 (Just 3)]
        it "succesfully parses two counters with a sample rate" $
            ("value:1|c|@3\nanotherval:1|c|@5" :: ByteString) ~> fullParser `shouldParse` [Metric Counter "value" 1 (Just 3), Metric Counter "anotherval" 1 (Just 5)]
        it "succesfully parses a gauge and a timer" $
            ("value:1|g\nvalue:1|ms" :: ByteString) ~> fullParser `shouldParse` [Metric Gauge "value" 1 Nothing, Metric Timer "value" 1 Nothing]
        it "should handle windows and unix newlines" $ do
            fullParser `shouldSucceedOn` ("value:1|g\r\nanotherval:1|g" :: ByteString)
            fullParser `shouldSucceedOn` ("value:1|g\nanotherval:1|g" :: ByteString)
        it "should be forgiving of a final missing newline" $
            ("value:1|g\nvalue:1|ms" :: ByteString) ~> fullParser `shouldParse` [Metric Gauge "value" 1 Nothing, Metric Timer "value" 1 Nothing]
        it "should not accept just a carriage return" $
            fullParser `shouldFailOn` ("value:1|g\ranotherval:1|g" :: ByteString)
    describe "metric parser - failing cases" $ do
        it "fails on an invalid metric type" $
            fullParser `shouldFailOn` ("value:1|z" :: ByteString)
        it "fails on an invalid seperators" $ do
            fullParser `shouldFailOn` ("value-1|g" :: ByteString)
            fullParser `shouldFailOn` ("value:1-g" :: ByteString)
        it "fails on an invalid value type" $
            fullParser `shouldFailOn` ("value:a|c" :: ByteString)
        it "fails with extra data not for a counter" $
            fullParser `shouldFailOn` ("value:0|g|@1" :: ByteString)

    describe "metric parser - conduit tests" $ do
        let useTheConduit :: [ByteString] -> ([Metric], Bool)
            useTheConduit input = let
              (exceptionOrUnit, output)
                    = runWriter $ runCatchT $ runConduit
                        $  DCL.sourceList input
                       =$= metricConduit2
                       =$= DCL.mapM_ (\metric -> tell [metric])

              in (output, isLeft exceptionOrUnit)

            shouldYieldCleanly input expected
                = useTheConduit input `shouldBe` (expected, False)

        it "returns a metric from a single packet" $
            ["value:1|c"] `shouldYieldCleanly` [Metric Counter "value" 1 Nothing]

        it "returns two metrics from a single packet" $
            ["value:1|c\nanotherval:2|c"] `shouldYieldCleanly`
            [ Metric Counter "value"      1 Nothing
            , Metric Counter "anotherval" 2 Nothing
            ]

        it "returns a metric split across two packets" $
            ["valu", "e:1|c"] `shouldYieldCleanly`
            [Metric Counter "value" 1 Nothing]

        it "doesn't stop parsing at the end of a packet even if it could do so" $
            ["value:1|c|@5", "0"] `shouldYieldCleanly`
            [Metric Counter "value" 1 (Just 50)]

        let shouldYieldBeforeThrowing input expected
                = useTheConduit input `shouldBe` (expected, True)

        it "returns values until it finds an parse error" $
            ["value:1|c\nanotherval:2|x\nyetanotherval:3|c"] `shouldYieldBeforeThrowing`
            [Metric Counter "value" 1 Nothing]

        it "returns no metrics from no packets" $
            [] `shouldYieldBeforeThrowing` []

        it "returns no metrics from an empty packet" $
            [""] `shouldYieldBeforeThrowing` []

        it "returns no metrics from some empty packets" $
            ["", ""] `shouldYieldBeforeThrowing` []

        it "can handle a parse error even before a newline" $
            ["value:1|cc"] `shouldYieldBeforeThrowing`
            [Metric Counter "value" 1 Nothing]
