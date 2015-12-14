module Statsd.Utils where

import Control.Concurrent.Async
import Control.Concurrent
import Data.List (nubBy)
import GHC.Conc (labelThread)
import System.IO (stderr)
import System.Log.Logger
import System.Log.Formatter
import System.Log.Handler.Simple (fileHandler, streamHandler)
import System.Log.Handler (setFormatter)


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
        stderrLevel = if verbose options then DEBUG else INFO
        logFormat = "$time - $loggername [$pid:'$tid'] - $prio - $msg"
    fh <- fileHandler file level >>= \lh -> return $
        setFormatter lh (simpleLogFormatter logFormat)
    sh <- streamHandler stderr stderrLevel >>= \lh -> return $
        setFormatter lh (simpleLogFormatter logFormat)

    updateGlobalLogger "" (setLevel DEBUG . setHandlers [fh, sh])

async' :: String -> IO a -> IO (Async a)
async' label act = async $ do
    tid <- myThreadId
    labelThread tid label
    act

waitForAll :: [Async a] -> IO ()
waitForAll = mapM_ wait
