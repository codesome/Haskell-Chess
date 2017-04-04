module VerifyMove.Queen where

import Types
import BoardUtils
import VerifyMove.Rook   as Rook
import VerifyMove.Bishop as Bishop

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state startCell endCell = do
  if startCell == endCell
    then False
  else do
    if ((Rook.verifyMove state startCell endCell == True) || (Bishop.verifyMove state startCell endCell == True)) == True
      then True
    else False
