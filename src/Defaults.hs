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

initialDisplayPoints :: [BoardSquare]
initialDisplayPoints = [
        ((-1.0,0.75,0),(0,0,0)), ((-0.75,0.75,0),(1,1,1)), ((-0.5,0.75,0),(0,0,0)), ((-0.25,0.75,0),(1,1,1)), ((0.0,0.75,0),(0,0,0)), ((0.25,0.75,0),(1,1,1)), ((0.5,0.75,0),(0,0,0)), ((0.75,0.75,0),(1,1,1)), 

        ((-1.0,0.5,0),(1,1,1)), ((-0.75,0.5,0),(0,0,0)), ((-0.5,0.5,0),(1,1,1)), ((-0.25,0.5,0),(0,0,0)), ((0.0,0.5,0),(1,1,1)), ((0.25,0.5,0),(0,0,0)), ((0.5,0.5,0),(1,1,1)), ((0.75,0.5,0),(0,0,0)), 

        ((-1.0,0.25,0),(0,0,0)), ((-0.75,0.25,0),(1,1,1)), ((-0.5,0.25,0),(0,0,0)), ((-0.25,0.25,0),(1,1,1)), ((0.0,0.25,0),(0,0,0)), ((0.25,0.25,0),(1,1,1)), ((0.5,0.25,0),(0,0,0)), ((0.75,0.25,0),(1,1,1)), 

        ((-1.0,0.0,0),(1,1,1)), ((-0.75,0.0,0),(0,0,0)), ((-0.5,0.0,0),(1,1,1)), ((-0.25,0.0,0),(0,0,0)), ((0.0,0.0,0),(1,1,1)), ((0.25,0.0,0),(0,0,0)), ((0.5,0.0,0),(1,1,1)), ((0.75,0.0,0),(0,0,0)), 

        ((-1.0,-0.25,0),(0,0,0)), ((-0.75,-0.25,0),(1,1,1)), ((-0.5,-0.25,0),(0,0,0)), ((-0.25,-0.25,0),(1,1,1)), ((0.0,-0.25,0),(0,0,0)), ((0.25,-0.25,0),(1,1,1)), ((0.5,-0.25,0),(0,0,0)), ((0.75,-0.25,0),(1,1,1)), 

        ((-1.0,-0.5,0),(1,1,1)), ((-0.75,-0.5,0),(0,0,0)), ((-0.5,-0.5,0),(1,1,1)), ((-0.25,-0.5,0),(0,0,0)), ((0.0,-0.5,0),(1,1,1)), ((0.25,-0.5,0),(0,0,0)), ((0.5,-0.5,0),(1,1,1)), ((0.75,-0.5,0),(0,0,0)), 

        ((-1.0,-0.75,0),(0,0,0)), ((-0.75,-0.75,0),(1,1,1)), ((-0.5,-0.75,0),(0,0,0)), ((-0.25,-0.75,0),(1,1,1)), ((0.0,-0.75,0),(0,0,0)), ((0.25,-0.75,0),(1,1,1)), ((0.5,-0.75,0),(0,0,0)), ((0.75,-0.75,0),(1,1,1)), 

        ((-1.0,-1.0,0),(1,1,1)), ((-0.75,-1.0,0),(0,0,0)), ((-0.5,-1.0,0),(1,1,1)), ((-0.25,-1.0,0),(0,0,0)), ((0.0,-1.0,0),(1,1,1)), ((0.25,-1.0,0),(0,0,0)), ((0.5,-1.0,0),(1,1,1)), ((0.75,-1.0,0),(0,0,0))

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
    blackKing=4,
    startPointIsSet=False,
    startPoint=0, 
    endPoint=0,
    boardPoints=initialDisplayPoints
}
