{-# LANGUAGE OverloadedStrings #-}
{- |
 - Module      :  $Header$
 - Copyright   :  (c) Adam Hodgen
 - License     :  <license>
 -
 - Stability   :  experimental
 - Portability :  unknown
 -
 - Parser for incoming StatsD metrics.
 - -}


-- module Statsd.Parser (metricProcessor) where
module Statsd.Parser where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString as S
import Data.ByteString.Char8 (ByteString, null)
import Data.Maybe (fromMaybe, isNothing)
import Data.Word (Word8)
import Prelude hiding (takeWhile, null)
import System.IO (Handle)

import Statsd.Metrics (Name, Value, SampleRate, Metric(..))

-- | Type to hold the state after parsing a metric message.
data MetricsMessage = MetricMsg
                        { metricName :: Name,
                          metricValue :: Value,
                          metricType :: ByteString,
                          metricExtra :: Maybe SampleRate }
                    deriving (Eq, Show)

metricProcessor :: Handle -> IO (Either String [Metric])
metricProcessor handle = do
    messages <- hGetMetricsMessage handle
    return $ Right $ fmap parseMetricsMessage messages

-- | Turn a 'MetricsMessage' into a 'Metric'
parseMetricsMessage :: MetricsMessage -> Metric
parseMetricsMessage (MetricMsg name value "c" (Just extra)) =
    Counter name value extra
parseMetricsMessage (MetricMsg name value "c" Nothing) =
    Counter name value 0.1

parseMetricsMessage (MetricMsg name value "ms" _) =
    Timer name value
parseMetricsMessage (MetricMsg name value "g" _) =
    Gauge name value
parseMetricsMessage (MetricMsg name value "h" _) =
    Histogram name value
parseMetricsMessage (MetricMsg name value "m" _) =
    Meter name value

-- | Receive data from the @handle@, return a list of 'MetricsMessage's
hGetMetricsMessage :: Handle -> IO [MetricsMessage]
hGetMetricsMessage handle = go S.empty
    where
        go rest = do
            parseResult <- parseWith readMore metricParser rest
            case parseResult of
                Fail extra _ s -> error $ "TODO Fail: " ++ s ++ "[" ++ show extra ++ "]"
                Partial {}     -> error "TODO Partial"
                Done extra result -> if null extra
                                        then error $ "extra data: " ++ show extra
                                        else return result

        readMore = S.hGetSome handle (4 * 1024)

-- | Parse any number of metrics
metricParser :: Parser [MetricsMessage]
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
partMetricParser :: Parser MetricsMessage
partMetricParser = do
    name <- takeWhile notColon
    colon
    value <- double
    pipe
    metricType <- parseType
    -- Might be a pipe and an extra, or a EOL
    next <- peekChar
    if fromMaybe ' ' next /= '|'
        then return $ MetricMsg name value metricType Nothing
        else
            if metricType /= "c"
                -- Extra data only on a counter
                then fail "extra data on non counter"
                else do
                    pipe
                    extra <- double
                    return $ MetricMsg name value metricType (Just extra)

-- Parsers

-- | Parse a statsd metric type
parseType :: Parser ByteString
parseType = string "c" <|>
            string "ms" <|>
            string "g" <|>
            string "h" <|>
            string "m" <?>
            "metricType"
{-
-- Maybe like this? Types might not like it
parseType :: Parser Metric
parseType = (string "c"  >> return Counter) <|>
            (string "ms" >> return Gauge)
-}

-- | Parse a single colon @:@ character.
colon :: Parser Char
colon = char ':'

isColon :: Char -> Bool
isColon c = c == ':'

notColon :: Char -> Bool
notColon = not . isColon

-- | Parse a single pipe (|) character.
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

