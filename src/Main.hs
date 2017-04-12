module Main (main) where

import Graphics.UI.GLUT
import UI.Bindings
import UI.Display
import Data.IORef

import Types
import BoardUtils
import GameUtils
import MoveUtils
import Defaults
import System.IO

import Network
import SocketHandlers
import Control.Concurrent

main :: IO ()
main =
    withSocketsDo $ do

        (_progName, _args) <- getArgsAndInitialize

        if length _args==4 then do
            
            let myport  = read $ (_args!!0) :: Int 
            let host    = (_args!!1)
            let port    = read $ (_args!!2) :: Int
            let mycolor = _args!!3
            
            sock <- listenOn $ PortNumber (toEnum myport::PortNumber)

            putStrLn "Starting server ..."
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

            initialWindowSize $= Size 600 600
            _ <- createWindow "Haskell Chess"

            let initGameState = if mycolor=="white"
                                then (enableMove initialGameStateW)
                                else (disableMove initialGameStateB)

            gameState <- newIORef initGameState

            if not (mycolor=="white")
                then do
                    forkIO $ opponentMoveHandler gameState sock
                    putStr ""
                else putStr ""

            -- move sender
            sender <- newIORef (sendMessageUtil host (PortNumber (toEnum port::PortNumber)))
            
            displayCallback       $= display gameState
            reshapeCallback       $= Just reshape
            keyboardMouseCallback $= Just (keyboardMouse gameState sock sender)
            mainLoop
        else
            putStrLn "$ haskell-chess {myport} {host} {port} {color}"
