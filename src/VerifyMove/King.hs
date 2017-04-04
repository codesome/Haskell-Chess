module VerifyMove.King where

import Types
import BoardUtils

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




