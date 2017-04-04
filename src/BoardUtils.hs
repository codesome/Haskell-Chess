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
    | p == PlayerW = "PlayerW"
    | otherwise    = "PlayerB"

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
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk 
    }) = b

getTurn :: GameState -> Player
getTurn (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk 
    }) = t

getWasCheck :: GameState -> Bool
getWasCheck (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk 
    }) = wc

getWhoWasInCheck :: GameState -> Maybe Player
getWhoWasInCheck (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk 
    }) = wwic

isInProgress :: GameState -> Bool
isInProgress (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk 
    }) = ip

getWhiteKingPos :: GameState -> Int
getWhiteKingPos (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk 
    }) = wk

getBlackKingPos :: GameState -> Int
getBlackKingPos (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk 
    }) = bk

getSquareAt :: GameState -> Int -> Square
getSquareAt state index =
    let board = getBoard state
        row = index `div` 8
        col = index `mod` 8
    in ((board !! row) !! col)

-- to set a square in the board
setSquareAt :: GameState -> Int -> Square -> GameState
setSquareAt (GameState { 
    board=board, turn=t, 
    wasCheck=wc, whoWasInCheck=wwic, 
    inProgress=ip, whiteKing=wk, 
    blackKing=bk }) pos square =
    let

        row = pos `div` 8
        col = pos `mod` 8

        (r1,_:r2) = splitAt row board
        (c1,_:c2) = splitAt col (board!!row)

        newState = GameState { 
            board= (r1 ++ (c1++(square:c2)):r2) , 
            turn=t,  wasCheck=wc, 
            whoWasInCheck=wwic, inProgress=ip,
            whiteKing=wk, blackKing=bk
        }

    in newState

setBlackKingPos :: GameState -> Int -> GameState
setBlackKingPos (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk 
    }) newPos = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=wk,  blackKing=newPos 
    })

setWhiteKingPos :: GameState -> Int -> GameState
setWhiteKingPos (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk 
    }) newPos = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=newPos,  blackKing=bk 
    })