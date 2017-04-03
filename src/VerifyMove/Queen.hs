module VerifyMove.Queen where

import Types
import BoardUtils
import VerifyMove.Rook   as Rook
import VerifyMove.Bishop as Bishop

queenTestState :: GameState
queenTestState = GameState {
    board = [
        [bRook, bKnight, bBishop, bQueen, bKing, bBishop, bKnight, bRook],
        [bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn],
        [wRook, wKnight, wBishop, wQueen, wKing, wBishop, wKnight, wRook]
    ],
    turn = PlayerW,
    wasCheck = False,
    whoWasInCheck = Nothing,
    inProgress = True
}

verifyMove :: GameState -> Int -> Int -> Bool
verifyMove state startCell endCell = do
  if startCell == endCell
    then False
  else do
    if ((Rook.verifyMove state startCell endCell == True) || (Bishop.verifyMove state startCell endCell == True)) == True
      then True
    else False
