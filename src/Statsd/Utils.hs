module Statsd.Utils where

import Data.List (nubBy)
import System.IO (stdout)
import System.Log.Logger
import System.Log.Handler.Simple (fileHandler, streamHandler)

import Statsd.Config


-- | Characters allowed in a metric name
allowedChars :: String
allowedChars = "_-." ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- | Replace invalid characters in a metric name
sanitizeKeyName :: String -> String
sanitizeKeyName =  stripInvalidChars . replaceSpaceSlash . squashSpaces
    where squashSpaces = nubBy (\x c -> c == ' ' && x == c)
          replaceSpaceSlash = map (\c -> if c == ' ' then '_' else if c == '/' then '-' else c)
          stripInvalidChars = filter (`elem` allowedChars)

-- | Configure the logging, with the given options
configureLogging options = do
    let level = logLevel options
        file  = logFile  options
        stdoutLevel = if verbose options then DEBUG else INFO
    fh <- fileHandler file level
    sh <- streamHandler stdout stdoutLevel
    updateGlobalLogger "" (setLevel DEBUG . setHandlers [fh, sh])
