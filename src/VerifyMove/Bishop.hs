module VerifyMove.Bishop where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state startCell endCell = do
  let   rowStart = startCell `div` 8
  let   colStart = startCell `mod` 8
  let   rowEnd   = endCell   `div` 8
  let   colEnd   = endCell   `mod` 8
  if (abs(rowStart-rowEnd)`div`(abs(colStart-colEnd))) == 1
    then do
      if startCell > endCell
        then do
          if checkDiagonalEmpty state (startCell-9) (endCell+9) == True
            then True
          else False
      else if endCell > startCell
        then do
        if checkDiagonalEmpty state (startCell+9) (endCell-9) == True
          then True
        else False
      else False
  else False

checkDiagonalEmpty :: GameState -> Int -> Int -> Bool
checkDiagonalEmpty state bCell eCell = do
    if eCell == bCell
      then do
        if getSquareAt state bCell == Empty
          then True
        else False
    else if eCell - bCell > 0
      then do
        if getSquareAt state bCell == Empty
          then checkDiagonalEmpty state (bCell+9) eCell
        else False
    else if bCell - eCell > 0
      then do
        if getSquareAt state bCell == Empty
          then checkDiagonalEmpty state (bCell-9) eCell
        else False
    else True
