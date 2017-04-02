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
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
        [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing],
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
    inProgress = True
}