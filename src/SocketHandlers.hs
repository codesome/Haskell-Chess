module SocketHandlers where

import Network
import System.IO

sendMessageUtil :: String -> PortID -> String -> IO ()
sendMessageUtil host port = sendMessage host port

sendMessage :: String -> PortID -> String -> IO ()
sendMessage host port message = withSocketsDo $ do
        handle <- connectTo host port
        hPutStr handle message
        hClose handle

handleMessage :: Socket -> (String -> IO ()) -> IO ()
handleMessage sock callback = do
    (handle, host, port) <- accept sock
    output <- hGetLine handle
    callback output
