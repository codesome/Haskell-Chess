module VerifyMove.King where

import Types
import BoardUtils

showBoardPlaces :: IO ()
showBoardPlaces = do
    putStrLn "+----+----+----+----+----+----+----+----+"
    putStrLn "| 00 | 01 | 02 | 03 | 04 | 05 | 06 | 07 |"
    putStrLn "+----+----+----+----+----+----+----+----+"
    putStrLn "| 08 | 09 | 10 | 11 | 12 | 13 | 14 | 15 |"
    putStrLn "+----+----+----+----+----+----+----+----+"
    putStrLn "| 16 | 17 | 18 | 19 | 20 | 21 | 22 | 23 |"
    putStrLn "+----+----+----+----+----+----+----+----+"
    putStrLn "| 24 | 25 | 26 | 27 | 28 | 29 | 30 | 31 |"
    putStrLn "+----+----+----+----+----+----+----+----+"
    putStrLn "| 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 |"
    putStrLn "+----+----+----+----+----+----+----+----+"
    putStrLn "| 40 | 41 | 42 | 43 | 44 | 45 | 46 | 47 |"
    putStrLn "+----+----+----+----+----+----+----+----+"
    putStrLn "| 48 | 49 | 50 | 51 | 52 | 53 | 54 | 55 |"
    putStrLn "+----+----+----+----+----+----+----+----+"
    putStrLn "| 56 | 57 | 58 | 59 | 60 | 61 | 62 | 63 |"
    putStrLn "+----+----+----+----+----+----+----+----+"

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




