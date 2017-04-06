module Main where

import Graphics.UI.GLUT
import UI.Bindings
import UI.Display
import Data.IORef


import Types
import BoardUtils
import DisplayUtils
import GameUtils
import MoveUtils
import Defaults

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialWindowSize $= Size 600 600
  _window <- createWindow "Haskell Chess"
  gameState <- newIORef initialGameState
  displayCallback $= display gameState
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse gameState)
  mainLoop

gameLoop :: GameState -> IO ()
gameLoop state = do
    print state
    putStr (getPlayerStr (getTurn state))
    putStrLn " Move"
    putStr "From: "

    fromStr <- getLine
    let from = read fromStr :: Int

    putStr "To: "
    toStr <- getLine
    let to = read toStr :: Int

    if (verifyMove state from to)
        then do
            let newState = togglePlayer (moveFromTo state from to)

            if True -- TODO: Check for check or checkmate
                then
                    -- TODO: Take actions for check or checkmate
                    gameLoop newState
                else
                    gameLoop newState


        else do
            putStrLn "Invalid Move!"
            gameLoop state

startGame :: IO ()
startGame = gameLoop initialGameState
