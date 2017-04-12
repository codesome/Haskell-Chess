module VerifyMove.Knight where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state start end =
    (\rowsMoved colsMoved -> 
        (rowsMoved /= 0) && (colsMoved /= 0) && ((rowsMoved+colsMoved)==3)
    ) (abs $ (start `div` 8) - (end `div` 8)) (abs $ (start `mod` 8) - (end `mod` 8))
