module GameUtils where


import Types
import BoardUtils
import DisplayUtils
import MoveUtils
import Defaults


gameUtilsBoard :: Board
gameUtilsBoard = [
        [bRook,  bKnight, bBishop, bQueen, bKing, bBishop,  bKnight, bRook],
        [bPawn,  bPawn,   bPawn,   bPawn,  Empty, bPawn,  bPawn,   bPawn],
        [Empty,  Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [Empty,  Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [Empty,  Empty,   Empty,   Empty,  wKing, Empty,   Empty,   Empty],
        [Empty,  Empty,   Empty,   Empty,  Empty, Empty,   Empty,   Empty],
        [wPawn,  wPawn,   wPawn,   Empty,  bPawn, bKnight,   Empty,   wPawn],
        [wRook, wKnight,  wBishop, wQueen, Empty, wBishop, wKnight, wRook]
    ]

checkGameState :: GameState
checkGameState = GameState {
        board = gameUtilsBoard,
        turn = PlayerW,
        wasCheck = False,
        whoWasInCheck = Nothing,
        inProgress = True,
        whiteKing=60,
        blackKing=4
}

colorCompliment :: PColor -> PColor
colorCompliment color1 = color2 where
  color2
      | color1 == White = Black
      | color1 == Black = White

checkForGameCheck :: GameState -> Int -> PColor -> Bool
checkForGameCheck state kingCell color = do
  if ((checkLeftColCheck state (kingCell-1) (colorCompliment color))||(checkRightColCheck state (kingCell+1) (colorCompliment color))||(checkDownRowCheck state (kingCell+8) (colorCompliment color))||
      (checkUpRowCheck state (kingCell-8) (colorCompliment color)) || (checkUpperLeftDiagonal state (kingCell-9) (colorCompliment color)) || (checkLowerLeftDiagonal state (kingCell+7) (colorCompliment color))||
      (checkLowerRightDiagonal state (kingCell+9) (colorCompliment color)) || (checkUpperRightDiagonal state (kingCell-7) (colorCompliment color))
      || (checkUMidLeftHorseCheck state kingCell (colorCompliment color)) || (checkLMidLeftHorseCheck state kingCell (colorCompliment color))
      || (checkUpperLeftHorseCheck state kingCell (colorCompliment color)) || (checkLowerLeftHorseCheck state kingCell (colorCompliment color))
      || (checkUMidLeftHorseCheck state kingCell (colorCompliment color))|| (checkUpperRightHorseCheck state kingCell (colorCompliment color))
      || (checkLowerRightHorseCheck state kingCell (colorCompliment color)) || (checkUMidRightHorseCheck state kingCell (colorCompliment color))
      || (checkLMidRightHorseCheck state kingCell (colorCompliment color)))
      then True
  else False

checkLeftColCheck :: GameState -> Int -> PColor -> Bool
checkLeftColCheck state cell color = do
    if ((cell `mod` 8) - 1 >= 0)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then checkLeftColCheck state (cell-1) color
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
    else False


checkRightColCheck :: GameState -> Int -> PColor -> Bool
checkRightColCheck state cell color = do
    if ((cell `mod` 8) + 1 <= 7)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then checkRightColCheck state (cell+1) color
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
    else False

checkDownRowCheck ::  GameState -> Int -> PColor -> Bool
checkDownRowCheck state cell color = do
    if ((cell `div` 8) <= 7)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then checkDownRowCheck state (cell+8) color
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
    else False

checkUpRowCheck ::  GameState -> Int -> PColor -> Bool
checkUpRowCheck state cell color = do
    if ((cell `div` 8) >= 0)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then checkUpRowCheck state (cell-8) color
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
    else False

checkUpperLeftDiagonal ::  GameState -> Int -> PColor -> Bool
checkUpperLeftDiagonal state cell color = do
    if ( (cell `div` 8) <= 7 && (cell `div` 8) >= 0 && (cell `mod` 8) - 1 >= 0)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then checkUpperLeftDiagonal state (cell-9) color
        else do
           if ((getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen || (getSquareType (getSquareAt state cell)) == Pawn)
             then True
           else False
    else False

checkLowerLeftDiagonal ::  GameState -> Int -> PColor -> Bool
checkLowerLeftDiagonal state cell color = do
    if ( (cell `div` 8) <= 7 && (cell `div` 8) >= 0 &&(cell `mod` 8) - 1 >= 0)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then checkLowerLeftDiagonal state (cell+7) color
        else do
           if ((getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen || (getSquareType (getSquareAt state cell)) == Pawn)
             then True
           else False
    else False

checkUpperRightDiagonal ::  GameState -> Int -> PColor -> Bool
checkUpperRightDiagonal state cell color = do
    if ( (cell `div` 8) <= 7 && (cell `div` 8) >= 0 &&(cell `mod` 8) + 1 <= 7)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then checkUpperRightDiagonal state (cell-7) color
        else do
           if ((getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen || (getSquareType (getSquareAt state cell)) == Pawn)
             then True
           else False
    else False

checkLowerRightDiagonal ::  GameState -> Int -> PColor -> Bool
checkLowerRightDiagonal state cell color = do
    if ( (cell `div` 8) <= 7 && (cell `div` 8) >= 0 &&(cell `mod` 8) + 1 <= 7)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then checkLowerRightDiagonal state (cell+9) color
        else do
           if ((getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen || (getSquareType (getSquareAt state cell)) == Pawn)
             then True
           else False
    else False

checkUpperLeftHorseCheck :: GameState -> Int -> PColor -> Bool
checkUpperLeftHorseCheck state cell color = do
    if (((cell-16) `div` 8)  >=0 && ((cell `mod` 8)-1) >= 0)
      then do
        if (getSquareColor (getSquareAt state (cell -17)) == color && (getSquareType (getSquareAt state (cell-17))) == Knight)
          then True
        else False
    else False

checkUMidLeftHorseCheck :: GameState -> Int -> PColor -> Bool
checkUMidLeftHorseCheck state cell color = do
    if ((cell-8) `div` 8 >=0 && ((cell `mod` 8)-2) >=0)
        then do
          if (getSquareColor (getSquareAt state (cell-10)) == color && (getSquareType (getSquareAt state (cell-10))) == Knight)
            then True
          else False
     else False

checkLMidLeftHorseCheck :: GameState -> Int -> PColor -> Bool
checkLMidLeftHorseCheck state cell color = do
       if (((cell `mod` 8)-2) >=0 && (cell+8) `div` 8 <=7)
           then do
             if (getSquareColor (getSquareAt state (cell+6)) == color && (getSquareType (getSquareAt state (cell+6))) == Knight)
               then True
             else False
        else False

checkLowerLeftHorseCheck :: GameState -> Int -> PColor -> Bool
checkLowerLeftHorseCheck state cell color = do
    if (((cell+16) `div` 8) <=7 && ((cell `mod` 8)-1) >=0)
        then do
          if (getSquareColor (getSquareAt state (cell+15)) == color && (getSquareType (getSquareAt state (cell+15))) == Knight)
            then True
          else False
     else False

checkUpperRightHorseCheck ::  GameState -> Int -> PColor -> Bool
checkUpperRightHorseCheck state cell color = do
    if ((cell-16) `div` 8 >=0 && ((cell `mod` 8)+1)<= 7)
      then do
        if (getSquareColor (getSquareAt state (cell -15)) == color && (getSquareType (getSquareAt state (cell-15))) == Knight)
          then True
        else False
    else False

checkUMidRightHorseCheck ::  GameState -> Int -> PColor -> Bool
checkUMidRightHorseCheck state cell color = do
    if ((cell-8) `div` 8 >=0 && ((cell `mod` 8)+2) <= 7)
        then do
          if (getSquareColor (getSquareAt state (cell-6)) == color && (getSquareType (getSquareAt state (cell-6))) == Knight)
            then True
          else False
     else False

checkLMidRightHorseCheck ::  GameState -> Int -> PColor -> Bool
checkLMidRightHorseCheck state cell color = do
       if (((cell `mod` 8)+2) >=0 && (cell+8) `div` 8 <=7)
           then do
             if (getSquareColor (getSquareAt state (cell+10)) == color && (getSquareType (getSquareAt state (cell+10))) == Knight)
               then True
             else False
        else False

checkLowerRightHorseCheck :: GameState -> Int -> PColor -> Bool
checkLowerRightHorseCheck state cell color = do
    if ((cell+16) `div` 8 <=7 && ((cell `mod` 8)+1) <=7)
        then do
          if (getSquareColor (getSquareAt state (cell+17)) == color && (getSquareType (getSquareAt state (cell+7))) == Knight)
            then True
          else False
     else False
