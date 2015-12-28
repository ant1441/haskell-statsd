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
module Statsd.Parser (individualMetricConduit, metricParser, metricConduit2) where

import Control.Applicative ((<|>), optional)
import Control.Monad (void)
import Data.Attoparsec.Combinator (sepBy)
import Data.Attoparsec.ByteString.Char8 ((<?>), char, choice, double, endOfInput, Parser, peekChar, string, takeWhile, endOfLine)
import Data.ByteString.Char8 (ByteString)
import Data.Maybe (fromMaybe, isNothing)
import Prelude hiding (takeWhile, null)

import Control.Monad.Catch (MonadThrow)
import Data.Conduit (Conduit, yield, (=$=))
import qualified Data.Conduit.List as DCL (map)
import Data.Conduit.Attoparsec (ParseError, conduitParserEither, PositionRange, conduitParser, sinkParser)

import Statsd.Metrics (Metric(Metric), MetricType(..))

metricConduit2 :: (Monad m, MonadThrow m) => Conduit ByteString m Metric
metricConduit2 = do
  firstMetric
  conduitParser subsequentMetrics =$= DCL.map snd

  where

  firstMetric = yield =<< sinkParser partMetricParser
  subsequentMetrics = separator >> partMetricParser

metricConduit :: Monad m => Conduit ByteString m (Either ParseError (PositionRange, [Metric]))
metricConduit = conduitParserEither metricParser

individualMetricConduit :: Monad m => Conduit ByteString m (Either ParseError (PositionRange, Metric))
individualMetricConduit = conduitParserEither individualMetricParser

individualMetricParser :: Parser Metric
individualMetricParser = partMetricParser <* maybeEndOfLine -- need EOL, not EOF between

-- | Parse any number of metrics
metricParser :: Parser [Metric]
metricParser = partMetricParser `sepBy` separator

separator :: Parser ()
separator = void $ optional (char '\r') >> char '\n'

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
      Counter -> optional $ pipe >> at >> double
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

