module VerifyMove.Bishop where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state startCell endCell
    | (startCell==endCell) || ( (abs (rowStart-rowEnd))/=(abs (colStart-colEnd)) ) = False
    | (((rowStart-rowEnd)`div`(colEnd-colStart))==1) = 
        (
            (startCell > endCell) 
            && (foldr andAll True  (map (isEmpty state) [(startCell-7),(startCell-14)..(endCell+7)]))
        ) || (
            (startCell < endCell) 
            && (foldr andAll True (map (isEmpty state) [(endCell-7),(endCell-14)..(startCell+7)]))
        )
    | otherwise = (
                  (startCell > endCell) 
                  && (foldr andAll True (map (isEmpty state) [(startCell-9),(startCell-18)..(endCell+9)]))
                ) || (
                  (startCell < endCell) 
                  && (foldr andAll True (map (isEmpty state) [(endCell-9),(endCell-18)..(startCell+9)]))
                )
    where
        rowStart = startCell `div` 8
        colStart = startCell `mod` 8
        rowEnd   = endCell   `div` 8
        colEnd   = endCell   `mod` 8

andAll :: Bool -> Bool -> Bool
andAll init x = (init && x)

isEmpty :: GameState -> Int -> Bool
isEmpty state index = ((getSquareAt state index) == Empty)