module Defaults (
    initialGameBoard,
    initialGameState
) where

import Types

-- Board at the start of the game
initialGameBoard :: Board
initialGameBoard = [
        [bRook, bKnight, bBishop, bQueen, bKing, bBishop, bKnight, bRook],
        [bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [wRook, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn],
        [wRook, wKnight, wBishop, wQueen, wKing, wBishop, wKnight, wRook]
    ]

-- GameState at the start of the game
initialGameState :: GameState
initialGameState = GameState {
    board = initialGameBoard,
    turn = PlayerW,
    wasCheck = False,
    whoWasInCheck = Nothing,
    inProgress = True,
    whiteKing=60,
    blackKing=4
}
