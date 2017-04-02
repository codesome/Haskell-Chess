module BoardUtils (
    getBoard,
    getTurn,
    getWasCheck,
    getWhoWasInCheck,
    isInProgress
) where

import Types

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