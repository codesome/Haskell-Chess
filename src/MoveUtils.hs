module MoveUtils where

import Types
import BoardUtils
import qualified VerifyMove.Bishop as Bishop
import qualified VerifyMove.King as King
import qualified VerifyMove.Knight as Knight
import qualified VerifyMove.Pawn as Pawn
import qualified VerifyMove.Queen as Queen
import qualified VerifyMove.Rook as Rook


-- board indices (will be removed later)
sbl :: IO ()
sbl = do
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

-- To check moves of PlayerW (will be removed later)
mtsw :: GameState
mtsw = GameState {
    board = [
        [bRook, bKnight, bBishop, bQueen, bKing, bBishop, bKnight, bRook],
        [Empty, bPawn,   bPawn,   Empty,  bPawn, Empty,   bPawn,   bPawn],
        [Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [Empty, wPawn,   wPawn,   Empty,  wPawn, Empty,   wPawn,   wPawn],
        [wRook, wKnight, wBishop, wQueen, wKing, wBishop, wKnight, wRook]
    ],
    turn = PlayerW,
    wasCheck = False,
    whoWasInCheck = Nothing,
    inProgress = True
}

-- To check moves of PlayerB (will be removed later)
mtsb :: GameState
mtsb = GameState {
    board = [
        [bRook, bKnight, bBishop, bQueen, bKing, bBishop, bKnight, bRook],
        [Empty, bPawn,   bPawn,   Empty,  bPawn, Empty,   bPawn,   bPawn],
        [Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [Empty, Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [Empty, wPawn,   wPawn,   Empty,  wPawn, Empty,   wPawn,   wPawn],
        [wRook, wKnight, wBishop, wQueen, wKing, wBishop, wKnight, wRook]
    ],
    turn = PlayerB,
    wasCheck = False,
    whoWasInCheck = Nothing,
    inProgress = True
}

-- Checks if a move is valid
verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state start end
    | validMove && (ptype==Bishop) = (Bishop.verifyMove state start end)
    | validMove && (ptype==King) = (King.verifyMove state start end)
    | validMove && (ptype==Knight) = (Knight.verifyMove state start end)
    | validMove && (ptype==Pawn) = (Pawn.verifyMove state start end)
    | validMove && (ptype==Queen) = (Queen.verifyMove state start end)
    | validMove && (ptype==Rook) = (Rook.verifyMove state start end)
    | otherwise = False
    where
        square = (getSquareAt state start)
        ptype = getSquareType square
        pcolor = getSquareColor square
        turn = getTurn state
        validMove =  (start>=0 && start<=63 && end>=0 && end<=63) && (((turn==PlayerW) && (pcolor==White)) || ((turn==PlayerB) && (pcolor==Black)))

-- to set a square in the board
setSquareAt :: GameState -> Int -> Square -> GameState
setSquareAt (GameState { board=board, turn=t, wasCheck=wc, whoWasInCheck=wwic, inProgress=ip }) pos square =
    let

        row = pos `div` 8
        col = pos `mod` 8

        (r1,_:r2) = splitAt row board
        (c1,_:c2) = splitAt col (board!!row)

        newState = GameState {
            board= (r1 ++ (c1++(square:c2)):r2) ,
            turn=t,
            wasCheck=wc,
            whoWasInCheck=wwic,
            inProgress=ip
        }

    in newState

-- Move a piece from 'from' to 'to' index
moveFromTo :: GameState -> Int -> Int -> GameState
moveFromTo state from to =
    let
       startSquare = getSquareAt state from
       intermediateState = setSquareAt state from (Empty) -- making from empty
       newState = setSquareAt intermediateState to startSquare -- making 'to' as that as 'from'
    in newState

togglePlayer :: GameState -> GameState
togglePlayer (GameState {
        board=b,
        turn=player,
        wasCheck=wc,
        whoWasInCheck=wwic,
        inProgress=ip
    }) =
        if player == PlayerW
            then (GameState {
                board=b,
                turn=PlayerB,
                wasCheck=wc,
                whoWasInCheck=wwic,
                inProgress=ip
            })

            else (GameState {
                board=b,
                turn=PlayerW,
                wasCheck=wc,
                whoWasInCheck=wwic,
                inProgress=ip
            })

{-
In game loop:
- verifyMove
- moveFromTo
- check for check
-}
