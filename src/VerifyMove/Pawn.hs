module VerifyMove.Pawn where

import Types
import BoardUtils

pawnTestState :: GameState 
pawnTestState = GameState {
    board = [
        [bRook, bKnight, bBishop, bQueen, bKing, bBishop, bKnight, bRook],
        [bPawn, bPawn, bPawn, Empty, bPawn, bPawn, bPawn, bPawn],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, bPawn, Empty, Empty, Empty, Empty],
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
verifyMove state start end = do
    let rowStart = start `div` 8
    let colStart = start `mod` 8
    let rowEnd = end `div` 8
    let colEnd = end `mod` 8

    let piece = getSquareAt state start
    if (piece /= Empty) && (rowStart/=rowEnd)
        then 

            if (getSquareColor piece)==White
                then -- White
                    (colStart==colEnd) -- same column 
                    && ( 
                        ((rowStart-rowEnd)==1) -- moved only 1 square
                        || ( 
                            rowStart==6 -- 7th row for white
                            && (rowStart-rowEnd)==2 -- moved only 2
                            && (getSquareAt state (40+colStart))==Empty -- in between was empty
                           ) -- moved 2 square
                       )
                else -- Black
                    (colStart==colEnd) -- same column
                    && ( 
                        ((rowEnd-rowStart)==1) -- moved only 1 square
                        || (
                            rowStart==1 -- 2nd row for black
                            && (rowEnd-rowStart)==2 -- moved only 2
                            && (getSquareAt state (16+colStart))==Empty -- in between was empty
                           ) -- moved 2 square
                       )

        else False