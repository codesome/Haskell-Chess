module VerifyMove.Bishop where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state startCell endCell = do
  let   rowStart = startCell `div` 8
  let   colStart = startCell `mod` 8
  let   rowEnd   = endCell   `div` 8
  let   colEnd   = endCell   `mod` 8
  if (startCell==endCell) || ( (abs (rowStart-rowEnd))/=(abs (colStart-colEnd)) )
    then False
    else 
      if ((rowStart-rowEnd) `div` (colEnd-colStart)) == 1
        then 
          (
            (startCell > endCell) 
            && (foldr foldfun True  (map isEmpty (map (getSquareAt state) [(startCell-7),(startCell-14)..(endCell+7)])))
          )
          || 
          (
            (startCell < endCell) 
            && (foldr foldfun True (map isEmpty (map (getSquareAt state) [(endCell-7),(endCell-14)..(startCell+7)])))
          )
        else 
          (
            (startCell > endCell) 
            && (foldr foldfun True (map isEmpty (map (getSquareAt state) [(startCell-9),(startCell-18)..(endCell+9)])))
          )
          || 
          (
            (startCell < endCell) 
            && (foldr foldfun True (map isEmpty (map (getSquareAt state) [(endCell-9),(endCell-18)..(startCell+9)])))
          )

foldfun :: Bool -> Bool -> Bool
foldfun inti x = (inti && x)

isEmpty :: Square -> Bool
isEmpty square = (square == Empty)