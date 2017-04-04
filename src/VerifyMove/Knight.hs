module VerifyMove.Knight where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state start end = do
    let rowsMoved = abs ((start `div` 8) - (end `div` 8))
    let colsMoved = abs ((start `mod` 8) - (end `mod` 8))

    (rowsMoved /= 0) && (colsMoved /= 0) && ((rowsMoved+colsMoved)==3)