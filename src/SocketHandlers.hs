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

printAndFlush :: String -> IO ()
printAndFlush msg = do
    putStrLn ("\x1b[32mopponent>\x1b[37m " ++ msg)
    hFlush stdout

handleChatMessage :: Socket -> IO ()
handleChatMessage sock = do
    (handle, host, port) <- accept sock
    output <- hGetLine handle
    printAndFlush output
    handleChatMessage sock

sendChatMessage :: String -> PortID -> IO ()
sendChatMessage host port = do 
        msg <- getLine
        putStrLn ("you> " ++ msg)
        withSocketsDo $ do
            handle <- connectTo host port
            hPutStr handle msg
            hClose handle
            sendChatMessage host port
