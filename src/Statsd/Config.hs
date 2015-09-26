module Statsd.Config where

import Options.Applicative
import Paths_statsd (version)
import Data.Version (showVersion)


data Options = Options
    { port :: Integer }

optParser :: Parser Options
optParser = Options
        <$> option auto
            ( value 8125
            <> short 'p'
            <> long "port"
            <> metavar "PORT"
            <> help "Port number to listen on" )
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
