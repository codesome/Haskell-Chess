module VerifyMove.King where

import Types
import BoardUtils

kingTestState :: GameState 
kingTestState = GameState {
    board = [
        [bRook, bKnight, bBishop, bQueen, bKing, bBishop, bKnight, bRook],
        [bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn],
        [wRook, wKnight, wBishop, wQueen, wKing, wBishop, wKnight, wRook]
    ],
    turn = PlayerW,
    wasCheck = False,
    whoWasInCheck = Nothing,
    inProgress = True
}

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state start end =
    let 
        rowStart = start `div` 8
        colStart = start `mod` 8
        rowEnd = end `div` 8
        colEnd = end `mod` 8
    in ((rowStart==rowEnd) && (abs (colStart-colEnd))==1) -- Same row
        || ((colStart==colEnd) && (abs (rowStart-rowEnd))==1) -- Same column
        || ((abs (rowStart-rowEnd))==1 && (abs (colStart-colEnd))==1) -- Moved diagonally




