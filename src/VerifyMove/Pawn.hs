module VerifyMove.Pawn where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state start end = do
    let rowStart = start `div` 8
    let colStart = start `mod` 8
    let rowEnd = end `div` 8
    let colEnd = end `mod` 8

    let piece = getSquareAt state start
    if (piece /= Empty) && (rowStart/=rowEnd)
        then 
            if (colStart==colEnd) -- same column 
            then ( 
                 ((rowStart-rowEnd)==1) -- moved only 1 square
                 || ( 
                     rowStart==6
                     && (rowStart-rowEnd)==2 -- moved only 2
                     && (getSquareAt state (40+colStart))==Empty -- in between was empty
                    ) -- moved 2 square
                )
            else 
                (
                    (abs (colStart-colEnd))==1
                    && (rowStart-rowEnd)==1
                    && (getSquareColor (getSquareAt state end))==Black
                )

        else False