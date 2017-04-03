module BoardUtils where

import Types

{- Square utils -}

getSquareColor :: Square -> PColor
getSquareColor (Piece pc pt) = pc

getSquareType :: Square -> PType
getSquareType (Piece pc pt) = pt

{- Getting string utils -}

getPlayerStr :: Player -> String
getPlayerStr p
    | p==PlayerW = "PlayerW"
    | otherwise = "PlayerB" 

getColorStr :: PColor -> String
getColorStr pc
    | pc==White = "White"
    | otherwise = "Black"

getPTypeStr :: PType -> String
getPTypeStr pt
    | pt==Bishop = "Bishop"
    | pt==King = "King"
    | pt==Knight = "Knight"
    | pt==Pawn = "Pawn"
    | pt==Queen = "Queen"
    | pt==Rook = "Rook"
    | otherwise = ""

describeSquare :: Square -> String
describeSquare (Piece pc pt) = (getColorStr pc) ++ " " ++ (getPTypeStr pt)
describeSquare (Empty) = "Empty Square"

{- GameState utils -}

getBoard :: GameState -> Board
getBoard (GameState {
    board=b, 
    turn=t, 
    wasCheck=wc, 
    whoWasInCheck=wwic, 
    inProgress=ip
    }) = b

getTurn :: GameState -> Player
getTurn (GameState {
    board=b, 
    turn=t, 
    wasCheck=wc, 
    whoWasInCheck=wwic, 
    inProgress=ip
    }) = t

getWasCheck :: GameState -> Bool
getWasCheck (GameState {
    board=b, 
    turn=t, 
    wasCheck=wc, 
    whoWasInCheck=wwic, 
    inProgress=ip
    }) = wc

getWhoWasInCheck :: GameState -> Maybe Player
getWhoWasInCheck (GameState {
    board=b, 
    turn=t, 
    wasCheck=wc, 
    whoWasInCheck=wwic, 
    inProgress=ip
    }) = wwic

isInProgress :: GameState -> Bool
isInProgress (GameState {
    board=b, 
    turn=t, 
    wasCheck=wc, 
    whoWasInCheck=wwic, 
    inProgress=ip
    }) = ip
