import Control.Concurrent (forkIO)
import qualified Data.ByteString as S
import Data.Either (lefts, rights)
import Network (accept, listenOn, withSocketsDo, HostName, Socket, PortID (..), PortNumber)
import System.IO (hClose, hPutStr, hSetBinaryMode, hSetBuffering, Handle, BufferMode(NoBuffering))

import Statsd.Config
import Statsd.Metrics
import Statsd.Parser

import Data.Typeable
import Debug.Trace

handleMetrics :: [Metric] -> IO ()
handleMetrics metrics = do
    error $ "TODO: handle " ++ (show . length) metrics ++ " metrics " ++ show metrics

reportError :: Handle -> String -> IO ()
reportError _ [] = trace "no errors" (return ())
reportError = hPutStr


traceType :: Typeable a => String -> a -> IO ()
traceType msg var = traceIO (msg ++ (show . typeOf) var)


connectionHandler :: Handle -> HostName -> PortNumber -> IO ()
connectionHandler handle hostname portNum = do
    putStrLn $ "Connection from " ++ hostname
    eitherMetrics <- metricProcessor handle
    case eitherMetrics of
        Left error -> reportError handle error
        Right metrics -> handleMetrics metrics
    hClose handle

sockHandler :: Socket -> IO ()
sockHandler sock = do
    (handle, hostname, portNum) <- accept sock
    hSetBuffering handle NoBuffering
    hSetBinaryMode handle True
    _ <- forkIO $ connectionHandler handle hostname portNum
    sockHandler sock

main = parseArgs start

start :: Options -> IO ()
start (Options portNum) = withSocketsDo $ do
    sock <- listenOn $ PortNumber $ fromInteger portNum
    putStrLn $ "Listening on port " ++ show portNum
    sockHandler sock
