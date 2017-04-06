module BoardUtils where

import Types
import Graphics.UI.GLUT (GLfloat)

{- Square utils -}

getSquareColor :: Square -> PColor
getSquareColor (Piece pc pt) = pc
getSquareColor (Empty) = NoColor

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
    board=b, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_
    }) = b

getTurn :: GameState -> Player
getTurn (GameState {
    board=_, turn=t, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_
    }) = t

getWasCheck :: GameState -> Bool
getWasCheck (GameState {
    board=_, turn=_, wasCheck=wc,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_
    }) = wc

getWhoWasInCheck :: GameState -> Maybe Player
getWhoWasInCheck (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=wwic, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_
    }) = wwic

isInProgress :: GameState -> Bool
isInProgress (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=ip,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_
    }) = ip

getWhiteKingPos :: GameState -> Int
getWhiteKingPos (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=wk,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_
    }) = wk

getBlackKingPos :: GameState -> Int
getBlackKingPos (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=bk , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_
    }) = bk

getStartPoint :: GameState -> Int
getStartPoint (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=sp, endPoint=_, boardPoints=_
    }) = sp

getEndPoint :: GameState -> Int
getEndPoint (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=ep, boardPoints=_
    }) = ep

getBoardPoints :: GameState -> [BoardSquare]
getBoardPoints (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=bp
    }) = bp

isStartPointSet :: GameState -> Bool
isStartPointSet (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=spis,
    startPoint=_, endPoint=_, boardPoints=_
    }) = spis

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
    blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp }) pos square =
    let

        row = pos `div` 8
        col = pos `mod` 8

        (r1,_:r2) = splitAt row board
        (c1,_:c2) = splitAt col (board!!row)

        newState = GameState { 
            board= (r1 ++ (c1++(square:c2)):r2) , 
            turn=t,  wasCheck=wc, 
            whoWasInCheck=wwic, inProgress=ip,
            whiteKing=wk, blackKing=bk , startPointIsSet=spis,
            startPoint=sp, endPoint=ep, boardPoints=bp
        }

    in newState

setBlackKingPos :: GameState -> Int -> GameState
setBlackKingPos (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep , boardPoints=bp
    }) newPos = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=wk,  blackKing=newPos , startPointIsSet=spis,
        startPoint=sp, endPoint=ep , boardPoints=bp
    })

setWhiteKingPos :: GameState -> Int -> GameState
setWhiteKingPos (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp
    }) newPos = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=newPos,  blackKing=bk , startPointIsSet=spis,
        startPoint=sp, endPoint=ep , boardPoints=bp
    })

setStartPointIsSet :: GameState -> Bool -> GameState
setStartPointIsSet (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp
    }) setIt = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=wk,  blackKing=bk , startPointIsSet=setIt,
        startPoint=sp, endPoint=ep , boardPoints=bp
    })

setStartPoint :: GameState -> Int -> GameState
setStartPoint (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp
    }) newSP = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
        startPoint=newSP, endPoint=ep , boardPoints=bp
    })

setEndPoint :: GameState -> Int -> GameState
setEndPoint (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp
    }) newEP = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
        startPoint=sp, endPoint=newEP , boardPoints=bp
    })

setBoardPoints :: GameState -> [BoardSquare] -> GameState
setBoardPoints (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp
    }) newBP = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
        startPoint=sp, endPoint=ep , boardPoints=newBP
    })

setBoardPointColorAt :: GameState -> Int -> (GLfloat,GLfloat,GLfloat) -> GameState
setBoardPointColorAt (GameState { 
    board=board, turn=t, 
    wasCheck=wc, whoWasInCheck=wwic, 
    inProgress=ip, whiteKing=wk, 
    blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp }) index newColor =
    let

        (l1, (coords,_):l2) = splitAt index bp

        newState = GameState { 
            board=board , 
            turn=t,  wasCheck=wc, 
            whoWasInCheck=wwic, inProgress=ip,
            whiteKing=wk, blackKing=bk , startPointIsSet=spis,
            startPoint=sp, endPoint=ep, boardPoints=(l1 ++ [(coords,newColor)] ++ l2)
        }

    in newState