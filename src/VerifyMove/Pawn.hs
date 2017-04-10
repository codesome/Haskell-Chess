module VerifyMove.Pawn where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> PColor -> Bool
verifyMove state start end col
    | (piece == Empty) || (rowStart == rowEnd) = False
    -- same column
    | (colStart==colEnd) = (getSquareAt state end)==Empty && ( 
                 ((rowStart-rowEnd)==1) -- moved only 1 square
                 || ( 
                     rowStart==6
                     && (rowStart-rowEnd)==2 -- moved only 2
                     && (getSquareAt state (40+colStart))==Empty -- in between was empty
                    ) -- moved 2 square
                )
    -- diagonal move
    | otherwise =  (
                    (abs (colStart-colEnd))==1
                    && (rowStart-rowEnd)==1
                    && (getSquareColor (getSquareAt state end))==oppcolor
                )
    where
        rowStart = start `div` 8
        colStart = start `mod` 8
        rowEnd = end `div` 8
        colEnd = end `mod` 8
        oppcolor = if col==White then Black else White
        piece = getSquareAt state start
