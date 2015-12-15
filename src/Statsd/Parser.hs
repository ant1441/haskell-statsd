{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune, ignore-exports #-}

-- |
-- Module      :  $Header$
-- Copyright   :  (c) Adam Hodgen
-- License     :  <license>
--
-- Stability   :  experimental
-- Portability :  unknown
--
-- Parser for incoming StatsD metrics.

--module Statsd.Parser (individualMetricConduit) where
-- TODO Exports for test
module Statsd.Parser (individualMetricConduit, metricParser) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8 ((<?>), char, choice, double, endOfInput, Parser, peekChar, string, takeWhile, option)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe, isNothing)
import Prelude hiding (takeWhile, null)

import Data.Conduit (Conduit)
import Data.Conduit.Attoparsec (ParseError, conduitParserEither, PositionRange)

import Statsd.Metrics (Metric(Metric), MetricType(..))


metricConduit :: Monad m => Conduit ByteString m (Either ParseError (PositionRange, [Metric]))
metricConduit = conduitParserEither metricParser

individualMetricConduit :: Monad m => Conduit ByteString m (Either ParseError (PositionRange, Metric))
individualMetricConduit = conduitParserEither individualMetricParser

individualMetricParser :: Parser Metric
individualMetricParser = do
    singleMetric <- partMetricParser
    maybeEndOfLine -- need EOL, not EOF between
    return singleMetric

-- | Parse any number of metrics
metricParser :: Parser [Metric]
metricParser = do
    singleMetric <- partMetricParser
    maybeEndOfLine -- need EOL, not EOF between
    next <- peekChar
    if isNothing next
        then return [singleMetric] -- base case
        else do
            metrics <- metricParser
            return $ singleMetric : metrics

-- <metric_name>:<value>|<type>|<extra>
-- | Parse a single statsd metric, not taking the line break at the end.
partMetricParser :: Parser Metric
partMetricParser = do
    name <- takeWhile notColon
    colon
    value <- double
    pipe
    metricType <- parseType
    maybeExtra <- case metricType of
      Counter -> option Nothing $ pipe >> at >> Just <$> double
      _       -> return Nothing
    return $ Metric metricType name value maybeExtra

-- | Parse a statsd metric type
parseType :: Parser MetricType
parseType = (string "c"  >> return Counter)   <|>
            (string "ms" >> return Timer)     <|>
            (string "g"  >> return Gauge)     <|>
            (string "h"  >> return Histogram) <|>
            (string "m"  >> return Meter)
            <?> "metricType"


-- * Simple Parsers

-- | Parse a single at @\@@ character.
at :: Parser Char
at = char '@'

isAt :: Char -> Bool
isAt c = c == '@'

notAt :: Char -> Bool
notAt = not . isAt

-- | Parse a single colon @:@ character.
colon :: Parser Char
colon = char ':'

isColon :: Char -> Bool
isColon c = c == ':'

notColon :: Char -> Bool
notColon = not . isColon

-- | Parse a single pipe @|@ character.
pipe :: Parser Char
pipe = char '|'

isPipe :: Char -> Bool
isPipe c = c == '|'

notPipe :: Char -> Bool
notPipe = not . isPipe

isNewline :: Char -> Bool
isNewline w = w == '\r' || w == '\n'

-- | Parse either an end of input or end of line folowed by end of input.
maybeEOLThenEndOfInput :: Parser ()
maybeEOLThenEndOfInput = maybeEndOfLine >> endOfInput <?> "[endOfLine] & endOfInput"

maybeEndOfLine :: Parser ByteString
maybeEndOfLine = choice [(char '\n' >> return "\n") <|> (string "\r\n" >> return "\r\n"), string ""]

