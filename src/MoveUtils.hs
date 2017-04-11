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
    | (ptype==Bishop) = (Bishop.verifyMove state start end)
    | (ptype==King) = (King.verifyMove state start end)
    | (ptype==Knight) = (Knight.verifyMove state start end)
    | (ptype==Pawn) = (Pawn.verifyMove state start end pcolor)
    | (ptype==Queen) = (Queen.verifyMove state start end)
    | (ptype==Rook) = (Rook.verifyMove state start end)
    | otherwise = False
    where
        square = (getSquareAt state start)
        ptype = getSquareType square
        pcolor = getSquareColor square
        turn = getTurn state
        validMove =  (start>=0 && start<=63 && end>=0 && end<=63) -- index in range
            && ( -- moved own piece
                ((turn==PlayerW) && (pcolor==White))
                || ((turn==PlayerB) && (pcolor==Black))
               )
            && (pcolor /= (getSquareColor (getSquareAt state end))) -- doesnt kill same color piece

-- Move a piece in 'boardPoints'
changePieceFromTo :: GameState -> Int -> Int -> GameState
changePieceFromTo state from to =
    let
        p = getBoardPieceAt state from
        newState = setBoardPieceAt (setBoardPieceAt state from emptyBoardPiece) to p
    in newState

-- Move a piece in 'board'
moveFromTo :: GameState -> Int -> Int -> GameState
moveFromTo state from to =
    let
       startSquare = getSquareAt state from
       intermediateState = setSquareAt state from (Empty)

       -- making 'to' as that as 'from' and updating king position
       newState = if (startSquare==wKing)
                        then setWhiteKingPos (setSquareAt intermediateState to startSquare) to
                        else if (startSquare==bKing)
                            then setBlackKingPos (setSquareAt intermediateState to startSquare) to
                            else setSquareAt intermediateState to startSquare

    in (changePieceFromTo newState from to)
