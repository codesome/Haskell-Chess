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

-- verifyMove :: GameState -> Int -> Int -> Bool

