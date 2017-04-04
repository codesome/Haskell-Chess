module Game where

import Types
import BoardUtils
import DisplayUtils
import GameUtils
import MoveUtils
import Defaults

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