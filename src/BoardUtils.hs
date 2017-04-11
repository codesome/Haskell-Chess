module BoardUtils where

import Types
import Graphics.UI.GLUT (GLfloat)

{- Square utils -}

getSquareColor :: Square -> PColor
getSquareColor (Piece pc pt) = pc
getSquareColor (Empty) = NoColor

getSquareType :: Square -> PType
getSquareType (Piece pc pt) = pt
getSquareType (Empty) = NoType

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
    startPoint=_, endPoint=_, boardPoints=_, moveEnabled=_
    }) = b

getTurn :: GameState -> Player
getTurn (GameState {
    board=_, turn=t, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_, moveEnabled=_
    }) = t

getWasCheck :: GameState -> Bool
getWasCheck (GameState {
    board=_, turn=_, wasCheck=wc,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_, moveEnabled=_
    }) = wc

getWhoWasInCheck :: GameState -> Maybe Player
getWhoWasInCheck (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=wwic, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_, moveEnabled=_
    }) = wwic

isInProgress :: GameState -> Bool
isInProgress (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=ip,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_, moveEnabled=_
    }) = ip

getKingPos :: GameState -> PColor -> Int
getKingPos state color 
    | color==White = getWhiteKingPos state
    | otherwise = getBlackKingPos state

getWhiteKingPos :: GameState -> Int
getWhiteKingPos (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=wk,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_, moveEnabled=_
    }) = wk

getBlackKingPos :: GameState -> Int
getBlackKingPos (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=bk , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_, moveEnabled=_
    }) = bk

getStartPoint :: GameState -> Int
getStartPoint (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=sp, endPoint=_, boardPoints=_, moveEnabled=_
    }) = sp

getEndPoint :: GameState -> Int
getEndPoint (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=ep, boardPoints=_, moveEnabled=_
    }) = ep

getBoardPoints :: GameState -> [BoardSquare]
getBoardPoints (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=bp, moveEnabled=_
    }) = bp

getMoveEnabled :: GameState -> Bool
getMoveEnabled (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, endPoint=_, boardPoints=_, moveEnabled=me
    }) = me

isStartPointSet :: GameState -> Bool
isStartPointSet (GameState {
    board=_, turn=_, wasCheck=_,
    whoWasInCheck=_, inProgress=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=spis,
    startPoint=_, endPoint=_, boardPoints=_, moveEnabled=_
    }) = spis

getSquareAt :: GameState -> Int -> Square
getSquareAt state index =
    let board = getBoard state
        row = index `div` 8
        col = index `mod` 8
    in ((board !! row) !! col)

getBoardPieceAt :: GameState -> Int -> (Int,GLfloat)
getBoardPieceAt state index =
    let (_,_,p) = ((getBoardPoints state) !! index)
    in p

-- to set a square in the board
setSquareAt :: GameState -> Int -> Square -> GameState
setSquareAt (GameState { 
    board=board, turn=t, 
    wasCheck=wc, whoWasInCheck=wwic, 
    inProgress=ip, whiteKing=wk, 
    blackKing=bk , startPointIsSet=spis, moveEnabled=me,
    startPoint=sp, endPoint=ep, boardPoints=bp }) pos square =
    let

        row = pos `div` 8
        col = pos `mod` 8

        (r1,_:r2) = splitAt row board
        (c1,_:c2) = splitAt col (board!!row)

        newState = GameState { 
            board= (r1 ++ (c1++(square:c2)):r2) , 
            turn=t,  wasCheck=wc, 
            whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
            whiteKing=wk, blackKing=bk , startPointIsSet=spis,
            startPoint=sp, endPoint=ep, boardPoints=bp
        }

    in newState

setBlackKingPos :: GameState -> Int -> GameState
setBlackKingPos (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep , boardPoints=bp
    }) newPos = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
        whiteKing=wk,  blackKing=newPos , startPointIsSet=spis,
        startPoint=sp, endPoint=ep , boardPoints=bp
    })

setWhiteKingPos :: GameState -> Int -> GameState
setWhiteKingPos (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp
    }) newPos = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
        whiteKing=newPos,  blackKing=bk , startPointIsSet=spis,
        startPoint=sp, endPoint=ep , boardPoints=bp
    })

setStartPointIsSet :: GameState -> Bool -> GameState
setStartPointIsSet (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp
    }) setIt = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
        whiteKing=wk,  blackKing=bk , startPointIsSet=setIt,
        startPoint=sp, endPoint=ep , boardPoints=bp
    })

setStartPoint :: GameState -> Int -> GameState
setStartPoint (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp
    }) newSP = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
        whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
        startPoint=newSP, endPoint=ep , boardPoints=bp
    })

setEndPoint :: GameState -> Int -> GameState
setEndPoint (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp
    }) newEP = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
        whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
        startPoint=sp, endPoint=newEP , boardPoints=bp
    })

setBoardPoints :: GameState -> [BoardSquare] -> GameState
setBoardPoints (GameState {
    board=b, turn=t, wasCheck=wc,
    whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp
    }) newBP = (GameState {
        board=b, turn=t, wasCheck=wc,
        whoWasInCheck=wwic, inProgress=ip, moveEnabled=me,
        whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
        startPoint=sp, endPoint=ep , boardPoints=newBP
    })

setBoardPointColorAt :: GameState -> Int -> (GLfloat,GLfloat,GLfloat) -> GameState
setBoardPointColorAt (GameState { 
    board=board, turn=t, 
    wasCheck=wc, whoWasInCheck=wwic, 
    inProgress=ip, whiteKing=wk,  moveEnabled=me,
    blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp }) index newColor =
    let

        (l1, (coords,_,p):l2) = splitAt index bp

        newState = GameState { 
            board=board , 
            turn=t,  wasCheck=wc,  moveEnabled=me,
            whoWasInCheck=wwic, inProgress=ip,
            whiteKing=wk, blackKing=bk , startPointIsSet=spis,
            startPoint=sp, endPoint=ep, boardPoints=(l1 ++ [(coords,newColor,p)] ++ l2)
        }

    in newState

setBoardPieceAt :: GameState -> Int -> (Int,GLfloat) -> GameState
setBoardPieceAt (GameState { 
    board=board, turn=t, 
    wasCheck=wc, whoWasInCheck=wwic, 
    inProgress=ip, whiteKing=wk,  moveEnabled=me,
    blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp }) index newPiece =
    let

        (l1, (coords,col,_):l2) = splitAt index bp

        newState = GameState { 
            board=board , 
            turn=t,  wasCheck=wc,  moveEnabled=me,
            whoWasInCheck=wwic, inProgress=ip,
            whiteKing=wk, blackKing=bk , startPointIsSet=spis,
            startPoint=sp, endPoint=ep, boardPoints=(l1 ++ [(coords,col,newPiece)] ++ l2)
        }

    in newState


enableMove :: GameState -> GameState
enableMove (GameState { 
    board=board, turn=t, 
    wasCheck=wc, whoWasInCheck=wwic, 
    inProgress=ip, whiteKing=wk,  moveEnabled=_,
    blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp }) 
    = (GameState { 
        board=board , 
        turn=t,  wasCheck=wc,  moveEnabled=True,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=wk, blackKing=bk , startPointIsSet=spis,
        startPoint=sp, endPoint=ep, boardPoints=bp
    })

disableMove :: GameState -> GameState
disableMove (GameState { 
    board=board, turn=t, 
    wasCheck=wc, whoWasInCheck=wwic, 
    inProgress=ip, whiteKing=wk,  moveEnabled=_,
    blackKing=bk , startPointIsSet=spis,
    startPoint=sp, endPoint=ep, boardPoints=bp })
    = (GameState {
        board=board , 
        turn=t,  wasCheck=wc,  moveEnabled=False,
        whoWasInCheck=wwic, inProgress=ip,
        whiteKing=wk, blackKing=bk , startPointIsSet=spis,
        startPoint=sp, endPoint=ep, boardPoints=bp
    })