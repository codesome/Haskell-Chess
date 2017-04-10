module MoveUtils where

import Types
import BoardUtils
import qualified VerifyMove.Bishop as Bishop
import qualified VerifyMove.King as King
import qualified VerifyMove.Knight as Knight
import qualified VerifyMove.Pawn as Pawn
import qualified VerifyMove.Queen as Queen
import qualified VerifyMove.Rook as Rook

-- Checks if a move is valid
verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state start end
    | validMove && (ptype==Bishop) = (Bishop.verifyMove state start end)
    | validMove && (ptype==King) = (King.verifyMove state start end)
    | validMove && (ptype==Knight) = (Knight.verifyMove state start end)
    | validMove && (ptype==Pawn) = (Pawn.verifyMove state start end pcolor)
    | validMove && (ptype==Queen) = (Queen.verifyMove state start end)
    | validMove && (ptype==Rook) = (Rook.verifyMove state start end)
    | otherwise = False
    where
        square = (getSquareAt state start)
        ptype = getSquareType square
        pcolor = getSquareColor square
        turn = getTurn state
        validMove =  (start>=0 && start<=63 && end>=0 && end<=63)
            && (
                ((turn==PlayerW) && (pcolor==White))
                || ((turn==PlayerB) && (pcolor==Black))
               )
            && (pcolor /= (getSquareColor (getSquareAt state end)))

changePieceFromTo :: GameState -> Int -> Int -> GameState
changePieceFromTo state from to =
    let
        p = getBoardPieceAt state from
        newState = setBoardPieceAt (setBoardPieceAt state from emptyBoardPiece) to p
    in newState

-- Move a piece from 'from' to 'to' index
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

togglePlayer :: GameState -> GameState
togglePlayer (GameState {
        board=b, turn=player, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=wk, blackKing=bk, startPointIsSet=spis,
        startPoint=sp, endPoint=ep, boardPoints=bp, moveEnabled=me
    })
    | (player == PlayerW) =  (GameState {
                board=b, turn=PlayerB, wasCheck=wc,
                whoWasInCheck=wwic, inProgress=ip,
                whiteKing=wk, blackKing=bk,startPointIsSet=spis,
                startPoint=sp, endPoint=ep, boardPoints=bp, moveEnabled=me
            })
    | otherwise = (GameState {
                board=b, turn=PlayerW, wasCheck=wc,
                whoWasInCheck=wwic, inProgress=ip,
                whiteKing=wk, blackKing=bk,startPointIsSet=spis,
                startPoint=sp, endPoint=ep, boardPoints=bp, moveEnabled=me
            })
