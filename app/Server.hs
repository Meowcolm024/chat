module Server where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fix              ( fix )
import           Data.Char                      ( isSpace )
import           Network.Socket
import           System.IO

main :: IO ()
main = do
    mySocket <- socket AF_INET Stream 0
    setSocketOption mySocket ReuseAddr 1
    bind mySocket (SockAddrInet 4242 0x0100007f)
    listen mySocket 2
    channel <- newChan
    -- discard messages in the original channel
    forkIO $ forever $ void $ readChan channel
    mainLoop mySocket channel 0

type Message = (Int, String)

mainLoop :: Socket -> Chan Message -> Int -> IO ()
mainLoop mySocket channel messageNum = do
    connection <- accept mySocket
    -- Association between a connection and a name   
    forkIO (runconnection connection channel messageNum)
    mainLoop mySocket channel $! messageNum + 1

-- Messages are sent to the user.
runconnection :: (Socket, SockAddr) -> Chan Message -> Int -> IO ()
runconnection (mySocket, _) channel messageNum = do
    let broadcast message = writeChan channel (messageNum, message)
    let trim = f . f where f = reverse . dropWhile isSpace
    myHandle <- socketToHandle mySocket ReadWriteMode
    hSetBuffering myHandle NoBuffering
    hPutStrLn myHandle "Welcome to the chat. Please choose a username: "
    userName <- trim <$> hGetLine myHandle
    broadcast ("--> " ++ userName ++ " in now online.")
    hPutStrLn myHandle ("Hello, " ++ userName ++ "!")
    commLine                <- dupChan channel
    -- Fork a thread which will read messages of the duplicated channel.
    readerFromDuplicateChan <- forkIO $ forever $ do
        (nextNum, line) <- readChan commLine
        when (messageNum /= nextNum) $ hPutStrLn myHandle line
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- hGetLine myHandle
        case trim line of   -- When an exception occurs, a message is sent and the loop is broken.
            "quit" -> hPutStrLn myHandle "Bye!"     -- If there is no exception, then continue looping.
            _      -> broadcast (userName ++ ": " ++ line) >> loop
        killThread readerFromDuplicateChan                          -- Kill after the loop ends      
        broadcast ("<-- " ++ userName ++ " is now offline.")        -- Send a last broadcast 
        hClose myHandle
