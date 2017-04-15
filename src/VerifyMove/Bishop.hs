module VerifyMove.Bishop where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state startCell endCell
    | (startCell==endCell) || ( (abs (rowStart-rowEnd))/=(abs (colStart-colEnd)) ) = False
    
    | (((rowStart-rowEnd) == (colEnd-colStart))) = 
        ( -- moving top right
            (startCell > endCell) 
            && (foldr (&&) True  (map (isEmpty state) [(startCell-7),(startCell-14)..(endCell+7)]))
        ) || ( -- moving bottom left
            (startCell < endCell) 
            && (foldr (&&) True (map (isEmpty state) [(endCell-7),(endCell-14)..(startCell+7)]))
        )
    
    | otherwise = 
        ( -- moving top left
            (startCell > endCell) 
            && (foldr (&&) True (map (isEmpty state) [(startCell-9),(startCell-18)..(endCell+9)]))
        ) || ( -- moving bottom right
            (startCell < endCell) 
            && (foldr (&&) True (map (isEmpty state) [(endCell-9),(endCell-18)..(startCell+9)]))
        )
    where
        rowStart = startCell `div` 8
        colStart = startCell `mod` 8
        rowEnd   = endCell   `div` 8
        colEnd   = endCell   `mod` 8

isEmpty :: GameState -> Int -> Bool
isEmpty state index = ((getSquareAt state index) == Empty)