module Defaults where

import Types

emptyRow :: [Square]
emptyRow = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]

-- initial game board for white
initialGameBoardW :: Board
initialGameBoardW = [
        [bRook, bKnight, bBishop, bQueen, bKing, bBishop, bKnight, bRook],
        [bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn],
        emptyRow, emptyRow, emptyRow, emptyRow,
        [wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn],
        [wRook, wKnight, wBishop, wQueen, wKing, wBishop, wKnight, wRook]
    ]

-- initial game board for black
initialGameBoardB :: Board
initialGameBoardB = [
        [wRook, wKnight, wBishop, wKing, wQueen, wBishop, wKnight, wRook],
        [wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn],
        emptyRow, emptyRow, emptyRow, emptyRow,
        [bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn],
        [bRook, bKnight, bBishop, bKing, bQueen, bBishop, bKnight, bRook]
    ]

-- 4 empty rows in between the pieces
initialEmptyDisplayPoints :: [BoardSquare]
initialEmptyDisplayPoints = [

        ((-1.0,0.25,0),(1,1,1),(0,0)), ((-0.75,0.25,0),(0,0,0),(0,0)), ((-0.5,0.25,0),(1,1,1),(0,0)), ((-0.25,0.25,0),(0,0,0),(0,0)), ((0.0,0.25,0),(1,1,1),(0,0)), ((0.25,0.25,0),(0,0,0),(0,0)), ((0.5,0.25,0),(1,1,1),(0,0)), ((0.75,0.25,0),(0,0,0),(0,0)), 

        ((-1.0,0.0,0),(0,0,0),(0,0)), ((-0.75,0.0,0),(1,1,1),(0,0)), ((-0.5,0.0,0),(0,0,0),(0,0)), ((-0.25,0.0,0),(1,1,1),(0,0)), ((0.0,0.0,0),(0,0,0),(0,0)), ((0.25,0.0,0),(1,1,1),(0,0)), ((0.5,0.0,0),(0,0,0),(0,0)), ((0.75,0.0,0),(1,1,1),(0,0)), 

        ((-1.0,-0.25,0),(1,1,1),(0,0)), ((-0.75,-0.25,0),(0,0,0),(0,0)), ((-0.5,-0.25,0),(1,1,1),(0,0)), ((-0.25,-0.25,0),(0,0,0),(0,0)), ((0.0,-0.25,0),(1,1,1),(0,0)), ((0.25,-0.25,0),(0,0,0),(0,0)), ((0.5,-0.25,0),(1,1,1),(0,0)), ((0.75,-0.25,0),(0,0,0),(0,0)), 

        ((-1.0,-0.5,0),(0,0,0),(0,0)), ((-0.75,-0.5,0),(1,1,1),(0,0)), ((-0.5,-0.5,0),(0,0,0),(0,0)), ((-0.25,-0.5,0),(1,1,1),(0,0)), ((0.0,-0.5,0),(0,0,0),(0,0)), ((0.25,-0.5,0),(1,1,1),(0,0)), ((0.5,-0.5,0),(0,0,0),(0,0)), ((0.75,-0.5,0),(1,1,1),(0,0))
    
    ]

-- initial display board points for white
initialDisplayPointsW :: [BoardSquare]
initialDisplayPointsW = [

            ((-1.0,0.75,0),(1,1,1),(6,0.05)), ((-0.75,0.75,0),(0,0,0),(3,0.05)), ((-0.5,0.75,0),(1,1,1),(1,0.05)), ((-0.25,0.75,0),(0,0,0),(5,0.05)), ((0.0,0.75,0),(1,1,1),(2,0.05)), ((0.25,0.75,0),(0,0,0),(1,0.05)), ((0.5,0.75,0),(1,1,1),(3,0.05)), ((0.75,0.75,0),(0,0,0),(6,0.05)), 

            ((-1.0,0.5,0),(0,0,0),(4,0.05)), ((-0.75,0.5,0),(1,1,1),(4,0.05)), ((-0.5,0.5,0),(0,0,0),(4,0.05)), ((-0.25,0.5,0),(1,1,1),(4,0.05)), ((0.0,0.5,0),(0,0,0),(4,0.05)), ((0.25,0.5,0),(1,1,1),(4,0.05)), ((0.5,0.5,0),(0,0,0),(4,0.05)), ((0.75,0.5,0),(1,1,1),(4,0.05))

        ] ++ initialEmptyDisplayPoints ++ [

            ((-1.0,-0.75,0),(1,1,1),(4,0.95)), ((-0.75,-0.75,0),(0,0,0),(4,0.95)), ((-0.5,-0.75,0),(1,1,1),(4,0.95)), ((-0.25,-0.75,0),(0,0,0),(4,0.95)), ((0.0,-0.75,0),(1,1,1),(4,0.95)), ((0.25,-0.75,0),(0,0,0),(4,0.95)), ((0.5,-0.75,0),(1,1,1),(4,0.95)), ((0.75,-0.75,0),(0,0,0),(4,0.95)), 

            ((-1.0,-1.0,0),(0,0,0),(6,0.95)), ((-0.75,-1.0,0),(1,1,1),(3,0.95)), ((-0.5,-1.0,0),(0,0,0),(1,0.95)), ((-0.25,-1.0,0),(1,1,1),(5,0.95)), ((0.0,-1.0,0),(0,0,0),(2,0.95)), ((0.25,-1.0,0),(1,1,1),(1,0.95)), ((0.5,-1.0,0),(0,0,0),(3,0.95)), ((0.75,-1.0,0),(1,1,1),(6,0.95))

        ]

-- initial display board points for black
initialDisplayPointsB :: [BoardSquare]
initialDisplayPointsB = [

            ((-1.0,0.75,0),(1,1,1),(6,0.95)), ((-0.75,0.75,0),(0,0,0),(3,0.95)), ((-0.5,0.75,0),(1,1,1),(1,0.95)), ((-0.25,0.75,0),(0,0,0),(2,0.95)), ((0.0,0.75,0),(1,1,1),(5,0.95)), ((0.25,0.75,0),(0,0,0),(1,0.95)), ((0.5,0.75,0),(1,1,1),(3,0.95)), ((0.75,0.75,0),(0,0,0),(6,0.95)), 

            ((-1.0,0.5,0),(0,0,0),(4,0.95)), ((-0.75,0.5,0),(1,1,1),(4,0.95)), ((-0.5,0.5,0),(0,0,0),(4,0.95)), ((-0.25,0.5,0),(1,1,1),(4,0.95)), ((0.0,0.5,0),(0,0,0),(4,0.95)), ((0.25,0.5,0),(1,1,1),(4,0.95)), ((0.5,0.5,0),(0,0,0),(4,0.95)), ((0.75,0.5,0),(1,1,1),(4,0.95))
        
        ] ++ initialEmptyDisplayPoints ++ [

            ((-1.0,-0.75,0),(1,1,1),(4,0.05)), ((-0.75,-0.75,0),(0,0,0),(4,0.05)), ((-0.5,-0.75,0),(1,1,1),(4,0.05)), ((-0.25,-0.75,0),(0,0,0),(4,0.05)), ((0.0,-0.75,0),(1,1,1),(4,0.05)), ((0.25,-0.75,0),(0,0,0),(4,0.05)), ((0.5,-0.75,0),(1,1,1),(4,0.05)), ((0.75,-0.75,0),(0,0,0),(4,0.05)), 

            ((-1.0,-1.0,0),(0,0,0),(6,0.05)), ((-0.75,-1.0,0),(1,1,1),(3,0.05)), ((-0.5,-1.0,0),(0,0,0),(1,0.05)), ((-0.25,-1.0,0),(1,1,1),(2,0.05)), ((0.0,-1.0,0),(0,0,0),(5,0.05)), ((0.25,-1.0,0),(1,1,1),(1,0.05)), ((0.5,-1.0,0),(0,0,0),(3,0.05)), ((0.75,-1.0,0),(1,1,1),(6,0.05))

        ]

-- GameState for white at the start of the game
initialGameStateW :: GameState
initialGameStateW = GameState {
    board = initialGameBoardW,
    turn            = PlayerW,
    wasCheck        = False,
    whoWasInCheck   = Nothing,
    inProgress      = True,
    whiteKing       =60,
    blackKing       =4,
    startPointIsSet =False,
    startPoint      =0, 
    endPoint        =0,
    boardPoints     =initialDisplayPointsW, 
    moveEnabled     =True
}

-- GameState for black at the start of the game
initialGameStateB :: GameState
initialGameStateB = GameState {
    board           = initialGameBoardB,
    turn            = PlayerB,
    wasCheck        = False,
    whoWasInCheck   = Nothing,
    inProgress      = True,
    whiteKing       =3,
    blackKing       =59,
    startPointIsSet =False,
    startPoint      =0, 
    endPoint        =0,
    boardPoints     = initialDisplayPointsB, 
    moveEnabled     =True
}
