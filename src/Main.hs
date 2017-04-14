module Main (main) where

import Graphics.UI.GLUT
import Data.IORef
import Network
import Control.Concurrent
import System.IO

import UI.Bindings
import UI.Display
import Types
import BoardUtils
import GameUtils
import MoveUtils
import Defaults
import SocketHandlers


main :: IO ()
main =
    withSocketsDo $ do

        (_, _args) <- getArgsAndInitialize

        if length _args==4 then do
            
            let myport  = read $ (_args!!0) :: Int 
            let host    = (_args!!1)
            let port    = read $ (_args!!2) :: Int
            let mycolor = _args!!3
            
            -- socket server
            sock <- listenOn $ PortNumber (toEnum myport::PortNumber)

            -------- Start up messages
            putStrLn $ "Starting server at port " ++ (show myport) ++ " ..."
            putStr "Press [Enter] when other player is ready"
            hFlush stdout
            _ <- getLine
            putStrLn "\x1b[35mEnter anything and press enter to send message to your opponent!\x1b[37m"

            ------- Initiating chat server
            -- to receive messages
            (listenOn $ PortNumber (toEnum (myport+1)::PortNumber)) >>=
                (\chatsock ->
                    forkIO $ handleChatMessage chatsock)
            
            -- to send messages
            forkIO $ sendChatMessage host (PortNumber (toEnum (port+1)::PortNumber))
            ------- / chat server


            -- selecting color
            let initGameState = if mycolor=="white"
                                then (enableMove initialGameStateW)
                                else (disableMove initialGameStateB)

            -- game state
            gameState <- newIORef initGameState

            if not (mycolor=="white")
                then do -- waiting for opponent
                    forkIO $ opponentMoveHandler gameState sock
                    putStr ""
                else putStr ""

            -- move sender (socket sender)
            sender <- newIORef (sendMessageUtil host (PortNumber (toEnum port::PortNumber)))
            
            -- initialising UI options
            initialWindowSize $= Size 600 600
            _ <- createWindow "Haskell Chess"
            displayCallback       $= display gameState
            reshapeCallback       $= Just reshape
            keyboardMouseCallback $= Just (keyboardMouse gameState sock sender)
            mainLoop
        else
            putStrLn "$ haskell-chess {myport} {host} {port} {color}\n\nHelp: https://github.com/thecodesome/Haskell-Chess"
