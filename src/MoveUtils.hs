module MoveUtils (
    verifyMove,
    moveFromTo
) where

import Types
import BoardUtils
import GameUtils
import qualified VerifyMove.Bishop as Bishop
import qualified VerifyMove.King as King
import qualified VerifyMove.Knight as Knight
import qualified VerifyMove.Pawn as Pawn
import qualified VerifyMove.Queen as Queen
import qualified VerifyMove.Rook as Rook

-- Checks if a move is valid
verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state start end
    | not validMove = False
    | otherwise = case ptype of  
                    Bishop    -> (Bishop.verifyMove state start end)
                    King      -> (King.verifyMove state start end)
                    Knight    -> (Knight.verifyMove state start end)
                    Pawn      -> (Pawn.verifyMove state start end pcolor)
                    Queen     -> (Queen.verifyMove state start end)
                    Rook      -> (Rook.verifyMove state start end)
                    otherwise -> False
    where
        square    = (getSquareAt state start) -- square at start
        ptype     = getSquareType square      -- type of square
        pcolor    = getSquareColor square     -- color of the square
        turn      = getTurn state             -- player who made the move
        validMove = (start>=0 && start<=63 && end>=0 && end<=63) -- index in range
            && ( -- moved own piece
                ((turn==PlayerW) && (pcolor==White))
                || ((turn==PlayerB) && (pcolor==Black))
               )
            && (pcolor /= (getSquareColor (getSquareAt state end))) -- doesnt kill same color piece

-- Move a piece in 'boardPoints'
changePieceFromTo :: GameState -> Int -> Int -> GameState
changePieceFromTo state from to = (\p -> 
            (setBoardPieceAt (setBoardPieceAt state from emptyBoardPiece) to p)
        ) $ getBoardPieceAt state from

-- Move a piece in 'board'
moveFromTo :: GameState -> Int -> Int -> GameState
moveFromTo state from to = (\startSquare intermediateState -> 
        (\newState -> (changePieceFromTo newState from to) ) $ -- moving piece on display board
            if (startSquare==wKing)
                then setWhiteKingPos (setSquareAt intermediateState to startSquare) to -- white king was moved
                else if (startSquare==bKing)
                    then setBlackKingPos (setSquareAt intermediateState to startSquare) to -- black king was moved
                    else setSquareAt intermediateState to startSquare
    ) (getSquareAt state from) (setSquareAt state from (Empty))
