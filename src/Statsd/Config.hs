module Statsd.Config where

import Options.Applicative
import qualified System.Log as Log
import System.FilePath (FilePath)

import Paths_statsd (version)
import Data.Version (showVersion)


data Options = Options
    { port          :: Integer,
      flushInterval :: Int,
      prefixStats   :: String,
      logFile       :: FilePath,
      logLevel      :: Log.Priority,
      verbose       :: Bool
    }

optParser :: Parser Options
optParser = Options
        <$> option auto
            ( value 8125
            <> short 'p'
            <> long "port"
            <> metavar "PORT"
            <> showDefault
            <> help "Port number to listen on" )
        <*> option auto
            ( value 5000000
            <> short 'f'
            <> long "flush"
            <> metavar "INTERVAL"
            <> showDefault
            <> help "Number of microseconds to wait between flushes" )
        <*> strOption
            ( value ""
            <> short 'p'
            <> long "prefixStats"
            <> metavar "PREFIX"
            <> help "The prefix to add to all stats" )
        <*> option auto
            ( value "statsd.log"
            <> long "log-file"
            <> metavar "FILE"
            <> help "The log file to log to" )
        <*> option auto
            ( value Log.INFO
            <> long "log-level"
            <> metavar "LEVEL"
            <> help "The log level to log at" )
        <*> switch
            ( long "verbose"
            <> help "Output more verbose info" )
        <**> infoOption (showVersion version)
            ( short 'v'
            <> long "version"
            <> help "Show the version" )

parseArgs func = execParser opts >>= func
    where
        opts = info (helper <*> optParser)
            ( fullDesc
            <> progDesc "Haskell clone of etsy's statsd"
            <> header "statsd header" )
