module VerifyMove.King where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state start end =
    (\ rowStart colStart rowEnd colEnd ->
        ((rowStart==rowEnd) && (abs (colStart-colEnd))==1)            -- Same row
        || ((colStart==colEnd) && (abs (rowStart-rowEnd))==1)         -- Same column
        || ((abs (rowStart-rowEnd))==1 && (abs (colStart-colEnd))==1) -- Moved diagonally
    ) (start`div`8) (start`mod`8) (end`div`8) (end`mod`8)
