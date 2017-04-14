module VerifyMove.Rook where

import Types
import BoardUtils

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state startCell endCell
    | (startCell == endCell) = False
    | (colStart == colEnd) = ((startCell > endCell)          -- moving above
                                && ((foldr (&&) True  (map (isEmpty state) [(startCell-8),(startCell-16)..(endCell+8)]))))
                             || ((not (startCell > endCell)) -- moving below
                                && (foldr (&&) True  (map (isEmpty state) [(startCell+8),(startCell+16)..(endCell-8)])))
    | (rowStart == rowEnd) = ((startCell > endCell)          -- moving left
                    && (foldr (&&) True  (map (isEmpty state) [(startCell-1),(startCell-2)..(endCell+1)])))
                  || ((not (startCell > endCell))            -- moving right
                    && (foldr (&&) True  (map (isEmpty state) [(startCell+1),(startCell+2)..(endCell-1)])))
    | otherwise = False
    where
        rowStart = startCell `div` 8
        colStart = startCell `mod` 8
        rowEnd   = endCell   `div` 8
        colEnd   = endCell   `mod` 8

isEmpty :: GameState -> Int -> Bool
isEmpty state index = ((getSquareAt state index) == Empty)