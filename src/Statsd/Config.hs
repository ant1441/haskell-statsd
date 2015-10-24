module Statsd.Config where

import Options.Applicative
import Paths_statsd (version)
import Data.Version (showVersion)


data Options = Options
    { port :: Integer,
      flushInterval :: Int,
      prefixStats :: String
    }

optParser :: Parser Options
optParser = Options
        <$> option auto
            ( value 8125
            <> short 'p'
            <> long "port"
            <> metavar "PORT"
            <> help "Port number to listen on" )
        <*> option auto
            ( value 5000000
            <> short 'f'
            <> long "flush"
            <> metavar "INTERVAL"
            <> help "Number of microseconds to wait between flushes" )
        <*> option auto
            ( value ""
            <> short 'p'
            <> long "prefixStats"
            <> metavar "PREFIX"
            <> help "" )
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
