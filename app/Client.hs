module Client where

import           Control.Concurrent
import           Control.Monad
import           Network.Socket
import           System.IO

main :: IO ()
main = do
    mySocket <- socket AF_INET Stream 0
    setSocketOption mySocket ReuseAddr 1
    connect mySocket (SockAddrInet 4242 0x0100007f)
    myHandle <- socketToHandle mySocket ReadWriteMode
    hSetBuffering myHandle NoBuffering
    -- read incoming message
    forkIO $ forever $ do
        msg <- hGetLine myHandle
        putStrLn msg
    -- write messages
    forever $ do
        msg <- getLine
        hPutStrLn myHandle msg
