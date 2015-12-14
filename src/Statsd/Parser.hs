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

module Statsd.Parser (metricProcessor, metricParser) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as S
import Data.ByteString.Char8 (ByteString, null)
import Data.Maybe (fromMaybe, isNothing)
import Data.Word (Word8)
import Prelude hiding (takeWhile, null)
import System.IO (Handle)

import Statsd.Metrics (Metric(Metric), MetricType(..))

metricProcessor :: Handle -> IO (Either String [Metric])
metricProcessor handle = do
    messages <- hGetMetricsMessage handle
    return $ Right messages

-- | Receive data from the @handle@, return a list of Metric's
hGetMetricsMessage :: Handle -> IO [Metric]
hGetMetricsMessage handle = go S.empty
    where
        go rest = do
            parseResult <- parseWith readMore metricParser rest
            case parseResult of
                Fail extra _ s -> error $ "TODO Fail: " ++ s ++ "[" ++ show extra ++ "]"
                Partial resumer -> error "TODO Partial"
                Done extra result -> if null extra
                                        then return result
                                        else error $ "extra data: " ++ show extra

        readMore = S.hGetSome handle (4 * 1024)

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
    -- Might be a pipe and an extra, or a EOL
    next <- peekChar
    if fromMaybe ' ' next /= '|'
        then return $ Metric metricType name value Nothing
        else
            if metricType /= Counter
                -- Extra data only on a counter
                then fail "extra data on non counter"
                else do
                    pipe
                    at
                    extra <- double
                    return $ Metric metricType name value (Just extra)

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

