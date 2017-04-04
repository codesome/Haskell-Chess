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
    then do
      if startCell > endCell
        then do
           if checkColEmpty state (startCell-8) (endCell+8) == True
             then True
           else False
      else do
        if checkColEmpty state (startCell+8) (endCell-8) == True
          then True
        else False
  else if rowStart == rowEnd
    then do
      if startCell > endCell
        then do
           if checkRowEmpty state (startCell-1) (endCell+1) == True
             then True
           else False
      else do
        if checkRowEmpty state (startCell+1) (endCell-1) == True
          then True
        else False
  else False

checkColEmpty :: GameState -> Int -> Int -> Bool
checkColEmpty state bCell eCell = do
  if eCell == bCell
    then do
      if getSquareAt state bCell == Empty
        then True
      else False
  else if eCell - bCell > 0
    then do
      if getSquareAt state bCell == Empty
        then checkColEmpty state (bCell+8) eCell
      else False
  else if bCell - eCell > 0
    then do
      if getSquareAt state bCell == Empty
        then checkColEmpty state (bCell-8) eCell
      else False
  else True

checkRowEmpty :: GameState -> Int -> Int -> Bool
checkRowEmpty state bCell eCell = do
  if eCell == bCell
    then do
      if getSquareAt state bCell == Empty
        then True
      else False
  else if eCell - bCell > 0
    then do
      if getSquareAt state bCell == Empty
        then checkRowEmpty state (bCell+1) eCell
      else False
  else if bCell - eCell > 0
    then do
      if getSquareAt state bCell == Empty
        then checkRowEmpty state (bCell-1) eCell
      else False
  else True
