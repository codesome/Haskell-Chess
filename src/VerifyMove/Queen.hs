module VerifyMove.Queen where

import Types
import BoardUtils
import VerifyMove.Rook   as Rook
import VerifyMove.Bishop as Bishop

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state startCell endCell = 
    (startCell /= endCell) 
    && ((Rook.verifyMove state startCell endCell) || (Bishop.verifyMove state startCell endCell))
