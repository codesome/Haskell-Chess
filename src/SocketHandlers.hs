module SocketHandlers (
    sendMessageUtil,
    handleMessage,
    handleChatMessage,
    sendChatMessage
) where

import Network
import System.IO

-------------- Game handlers
-- Gives function to send next move with host and port set 
sendMessageUtil :: String -> PortID -> String -> IO ()
sendMessageUtil host port = sendMessage host port

-- Function to send next move
sendMessage :: String -> PortID -> String -> IO ()
sendMessage host port message = withSocketsDo $ do
        handle <- connectTo host port
        hPutStr handle message
        hClose handle

-- To handle next move by opponent
handleMessage :: Socket -> (String -> IO ()) -> IO ()
handleMessage sock callback = do
    (handle, host, port) <- accept sock
    output <- hGetLine handle
    callback output -- callback is used on received message


-------------- Chat handlers

-- to send chat message
sendChatMessage :: String -> PortID -> IO ()
sendChatMessage host port = do 
        msg <- getLine
        putStrLn ("you> " ++ msg)
        withSocketsDo $ do
            handle <- connectTo host port
            hPutStr handle msg
            hClose handle
            sendChatMessage host port

-- To receive chat message
handleChatMessage :: Socket -> IO ()
handleChatMessage sock = do
    (handle, host, port) <- accept sock
    output <- hGetLine handle
    printAndFlush output
    handleChatMessage sock

printAndFlush :: String -> IO ()
printAndFlush msg = do
    putStrLn ("\x1b[32mopponent>\x1b[37m " ++ msg)
    hFlush stdout
