module BoardUtils where

import Graphics.UI.GLUT (GLfloat)

import Types

{- Square utils -}

-- get color of a square
getSquareColor :: Square -> PColor
getSquareColor (Piece pc pt) = pc
getSquareColor (Empty)       = NoColor

-- get type of a square
getSquareType :: Square -> PType
getSquareType (Piece pc pt) = pt
getSquareType (Empty)       = NoType

getPTypeStr :: PType -> String
getPTypeStr pt = case pt of
    Bishop    -> "Bishop"
    King      -> "King"
    Knight    -> "Knight"
    Pawn      -> "Pawn"
    Queen     -> "Queen"
    Rook      -> "Rook"
    otherwise -> "NoType"

{-/ Square utils -}

{- GameState utils -}

-------------- GET

-- 'board'
getBoard :: GameState -> Board
getBoard (GameState {
    board=b, turn=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, boardPoints=_, moveEnabled=_
    }) = b

-- 'turn'
getTurn :: GameState -> Player
getTurn (GameState {
    board=_, turn=t,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, boardPoints=_, moveEnabled=_
    }) = t

-- 'whiteKing' or 'blackKing'
getKingPos :: GameState -> PColor -> Int
getKingPos state color
    | color==White = getWhiteKingPos state
    | otherwise    = getBlackKingPos state

-- 'whiteKing'
getWhiteKingPos :: GameState -> Int
getWhiteKingPos (GameState {
    board=_, turn=_,
    whiteKing=wk,  blackKing=_ , startPointIsSet=_,
    startPoint=_, boardPoints=_, moveEnabled=_
    }) = wk

-- 'blackKing'
getBlackKingPos :: GameState -> Int
getBlackKingPos (GameState {
    board=_, turn=_,
    whiteKing=_,  blackKing=bk , startPointIsSet=_,
    startPoint=_, boardPoints=_, moveEnabled=_
    }) = bk

-- 'startPoint'
getStartPoint :: GameState -> Int
getStartPoint (GameState {
    board=_, turn=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=sp, boardPoints=_, moveEnabled=_
    }) = sp

-- 'boardPoints'
getBoardPoints :: GameState -> [BoardSquare]
getBoardPoints (GameState {
    board=_, turn=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, boardPoints=bp, moveEnabled=_
    }) = bp

-- 'moveEnabled'
getMoveEnabled :: GameState -> Bool
getMoveEnabled (GameState {
    board=_, turn=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=_,
    startPoint=_, boardPoints=_, moveEnabled=me
    }) = me

-- 'startPointIsSet'
isStartPointSet :: GameState -> Bool
isStartPointSet (GameState {
    board=_, turn=_,
    whiteKing=_,  blackKing=_ , startPointIsSet=spis,
    startPoint=_, boardPoints=_, moveEnabled=_
    }) = spis

-- get square from 'board' at given index (0-63)
getSquareAt :: GameState -> Int -> Square
getSquareAt state index = (\board row col ->
        ((board !! row) !! col)
    ) (getBoard state) (index `div` 8) (index `mod` 8)

-- get square from 'board' at given index (0-63)
getSquareColorAt :: GameState -> Int -> PColor
getSquareColorAt state index = (\board row col ->
        getSquareColor ((board !! row) !! col)
    ) (getBoard state) (index `div` 8) (index `mod` 8)

-- get display point from 'boardPoints' at given index (0-63)
getBoardPieceAt :: GameState -> Int -> (Int,GLfloat)
getBoardPieceAt state index = (\(_,_,p) -> p) ((getBoardPoints state) !! index)


-------------- SET

-- update square at an index
setSquareAt :: GameState -> Int -> Square -> GameState
setSquareAt (GameState { 
    board=board, turn=t, whiteKing=wk, 
    blackKing=bk , startPointIsSet=spis, moveEnabled=me,
    startPoint=sp, boardPoints=bp }) pos square =
    (\(r1,_:r2) (c1,_:c2) ->
        (GameState { 
            board= (r1 ++ (c1++(square:c2)):r2) , 
            turn=t,  moveEnabled=me,
            whiteKing=wk, blackKing=bk , startPointIsSet=spis,
            startPoint=sp, boardPoints=bp
        })
    ) (splitAt (pos`div`8) board) (splitAt (pos`mod`8) (board!!(pos`div`8)))

-- update black king position
setBlackKingPos :: GameState -> Int -> GameState
setBlackKingPos (GameState {
    board=b, turn=t, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, boardPoints=bp
    }) newPos = (GameState {
        board=b, turn=t, moveEnabled=me,
        whiteKing=wk,  blackKing=newPos , startPointIsSet=spis,
        startPoint=sp, boardPoints=bp
    })

-- update white king position
setWhiteKingPos :: GameState -> Int -> GameState
setWhiteKingPos (GameState {
    board=b, turn=t, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, boardPoints=bp
    }) newPos = (GameState {
        board=b, turn=t, moveEnabled=me,
        whiteKing=newPos,  blackKing=bk , startPointIsSet=spis,
        startPoint=sp, boardPoints=bp
    })

-- set if the start point is set is True or False
setStartPointIsSet :: GameState -> Bool -> GameState
setStartPointIsSet (GameState {
    board=b, turn=t, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, boardPoints=bp
    }) setIt = (GameState {
        board=b, turn=t, moveEnabled=me,
        whiteKing=wk,  blackKing=bk , startPointIsSet=setIt,
        startPoint=sp, boardPoints=bp
    })

-- 'startPoint'
setStartPoint :: GameState -> Int -> GameState
setStartPoint (GameState {
    board=b, turn=t, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, boardPoints=bp
    }) newSP = (GameState {
        board=b, turn=t, moveEnabled=me,
        whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
        startPoint=newSP, boardPoints=bp
    })

-- 'boardPoints'
setBoardPoints :: GameState -> [BoardSquare] -> GameState
setBoardPoints (GameState {
    board=b, turn=t, moveEnabled=me,
    whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
    startPoint=sp, boardPoints=bp
    }) newBP = (GameState {
        board=b, turn=t, moveEnabled=me,
        whiteKing=wk,  blackKing=bk , startPointIsSet=spis,
        startPoint=sp, boardPoints=newBP
    })

-- set color of point in 'boardPoints' at given index
setBoardPointColorAt :: GameState -> Int -> (GLfloat,GLfloat,GLfloat) -> GameState
setBoardPointColorAt (GameState { 
    board=board, turn=t, whiteKing=wk,  moveEnabled=me,
    blackKing=bk , startPointIsSet=spis,
    startPoint=sp, boardPoints=bp }) index newColor =
    (\(l1, (coords,_,p):l2) -> 
        (GameState { 
            board=board, turn=t, moveEnabled=me,
            whiteKing=wk, blackKing=bk , startPointIsSet=spis,
            startPoint=sp, boardPoints=(l1 ++ [(coords,newColor,p)] ++ l2)
        })
    ) $ splitAt index bp

-- set 'boardPoints' at given index
setBoardPieceAt :: GameState -> Int -> (Int,GLfloat) -> GameState
setBoardPieceAt (GameState { 
    board=board, turn=t, whiteKing=wk,  moveEnabled=me,
    blackKing=bk , startPointIsSet=spis,
    startPoint=sp, boardPoints=bp }) index newPiece =
    (\(l1, (coords,col,_):l2) -> 
        (GameState { 
            board=board, moveEnabled=me, turn=t,
            whiteKing=wk, blackKing=bk , startPointIsSet=spis,
            startPoint=sp, boardPoints=(l1 ++ [(coords,col,newPiece)] ++ l2)
        })
    ) $ splitAt index bp

-- enable the moves
enableMove :: GameState -> GameState
enableMove (GameState { 
    board=board, turn=t, whiteKing=wk,  moveEnabled=_,
    blackKing=bk , startPointIsSet=spis,
    startPoint=sp, boardPoints=bp }) 
    = (GameState { 
        board=board, turn=t,  moveEnabled=True,
        whiteKing=wk, blackKing=bk , startPointIsSet=spis,
        startPoint=sp, boardPoints=bp
    })

-- disable the moves
disableMove :: GameState -> GameState
disableMove (GameState { 
    board=board, turn=t, whiteKing=wk,  moveEnabled=_,
    blackKing=bk , startPointIsSet=spis,
    startPoint=sp, boardPoints=bp })
    = (GameState {
        board=board, turn=t,  moveEnabled=False,
        whiteKing=wk, blackKing=bk , startPointIsSet=spis,
        startPoint=sp, boardPoints=bp
    })