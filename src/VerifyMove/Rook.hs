module VerifyMove.Rook where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state startCell endCell = do
  let   rowStart = startCell `div` 8
  let   colStart = startCell `mod` 8
  let   rowEnd   = endCell   `div` 8
  let   colEnd   = endCell   `mod` 8
  if (startCell == endCell)
    then False
  else if colStart == colEnd
    then
      if startCell > endCell
        then (foldr foldfun True  (map isEmpty (map (getSquareAt state) [(startCell-8),(startCell-16)..(endCell+8)])))
      else (foldr foldfun True  (map isEmpty (map (getSquareAt state) [(startCell+8),(startCell+16)..(endCell-8)])))
  else if rowStart == rowEnd
    then
      if startCell > endCell
        then (foldr foldfun True  (map isEmpty (map (getSquareAt state) [(startCell-1),(startCell-2)..(endCell+1)])))
      else (foldr foldfun True  (map isEmpty (map (getSquareAt state) [(startCell+1),(startCell+2)..(endCell-1)])))
  else False

foldfun :: Bool -> Bool -> Bool
foldfun inti x = (inti && x)

isEmpty :: Square -> Bool
isEmpty square = (square == Empty)