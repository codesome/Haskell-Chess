module GameUtils where


import Types
import BoardUtils
import DisplayUtils
import MoveUtils
import Defaults


gameUtilsBoard :: Board
gameUtilsBoard = [
        [Empty, bKnight, bBishop, bQueen, bKing, bBishop, bKnight, bRook],
        [bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn, bPawn],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [bRook, Empty, Empty, Empty, wKing, Empty, Empty, Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty],
        [wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn, wPawn],
        [wRook, wKnight, wBishop, wQueen, Empty, wBishop, wKnight, wRook]
    ]

checkGameState :: GameState
checkGameState = GameState {
        board = gameUtilsBoard,
        turn = PlayerW,
        wasCheck = False,
        whoWasInCheck = Nothing,
        inProgress = True,
        whiteKing=36,
        blackKing=4,
        startPointIsSet=False,
        startPoint=0, 
        endPoint=0,
        boardPoints=initialDisplayPoints
}

colorCompliment :: PColor -> PColor
colorCompliment color1 = color2 where
  color2
      | color1 == White = Black
      | color1 == Black = White

checkForGameCheck :: GameState -> Int -> PColor -> Bool
checkForGameCheck state kingCell color = do
  if ((checkLeftColCheck state (kingCell) (colorCompliment color))
      )
      then True
  else False

checkForPawn :: GameState -> Int -> PColor -> Bool
checkForPawn state cell color = do
  if ((cell-7) `div` 8 >= 0 || (cell-9) `div` 8 >= 0)
    then do
      if (getSquareColor (getSquareAt state (cell-7)) == color && (getSquareType (getSquareAt state  (cell-7))) == Pawn)
        then True
      else if (getSquareColor (getSquareAt state (cell-9)) == color && (getSquareType (getSquareAt state  (cell-9))) == Pawn)
        then True
      else False
  else False

checkLeftColCheck :: GameState -> Int -> PColor -> Bool
checkLeftColCheck state cell color = do
    if (((getSquareType(getSquareAt state cell)) == King) && ((getSquareColor(getSquareAt state cell)) == colorCompliment color))
      then do
        if cell `mod` 8 == 0 then False
        else checkLeftColCheck state (cell-1) color
    else if ((cell `mod` 8)>= 0 && cell >= 0)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then do
            if (cell `mod` 8) /= 0
              then checkLeftColCheck state (cell-1) color
            else False
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
    else False

checkRightColCheck :: GameState -> Int -> PColor -> Bool
checkRightColCheck state cell color = do
  if (((getSquareType(getSquareAt state cell)) == King) && ((getSquareColor(getSquareAt state cell)) == colorCompliment color))
    then do
      if cell `mod` 8 == 7
        then False
      else checkRightColCheck state (cell+7) color
  else if (cell <= 63 && (cell `mod` 8) <= 7)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then do
            if (cell `mod` 8) /= 7
              then checkRightColCheck state (cell+1) color
            else False
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
    else False

checkDownRowCheck ::  GameState -> Int -> PColor -> Bool
checkDownRowCheck state cell color = do
  if (((getSquareType(getSquareAt state cell)) == King) && ((getSquareColor(getSquareAt state cell)) == colorCompliment color))
    then do
      if cell `div` 8 == 7
        then False
      else checkDownRowCheck state (cell+8) color
  else if ((cell `div` 8) < 8)
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
  if (((getSquareType(getSquareAt state cell)) == King) && ((getSquareColor(getSquareAt state cell)) == colorCompliment color))
    then do
      if cell `div` 8 == 0
        then False
      else checkUpRowCheck state (cell-8) color
  else if ((cell `div` 8) >= 0 )
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
  if (((getSquareType(getSquareAt state cell)) == King) && ((getSquareColor(getSquareAt state cell)) == colorCompliment color))
    then do
      if cell `mod` 8 == 0
        then False
      else checkUpperLeftDiagonal state (cell-9) color
  else if ( (cell `div` 8) < 8 && (cell `div` 8) >= 0  && (cell `mod` 8) >= 0)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then do
            if (cell `mod` 8) /= 0
              then checkUpperLeftDiagonal state (cell-9) color
            else False
        else do
           if ((getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen )
             then True
           else False
    else False

checkLowerLeftDiagonal ::  GameState -> Int -> PColor -> Bool
checkLowerLeftDiagonal state cell color = do
  if (((getSquareType(getSquareAt state cell)) == King) && ((getSquareColor(getSquareAt state cell)) == colorCompliment color))
    then do
      if cell `mod` 8 == 0
        then False
      else checkLowerLeftDiagonal state (cell+7) color
  else if (((cell `div` 8) < 8) && ((cell `div` 8) >= 0) && (cell `mod` 8 >= 0))
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then do
            if (cell `mod` 8) /= 0
              then checkLowerLeftDiagonal state (cell+7) color
            else False
        else do
           if ((getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen )
             then True
           else False
    else False

checkUpperRightDiagonal ::  GameState -> Int -> PColor -> Bool
checkUpperRightDiagonal state cell color = do
  if (((getSquareType(getSquareAt state cell)) == King) && ((getSquareColor(getSquareAt state cell)) == colorCompliment color))
    then do
      if cell `mod` 8 == 7
        then False
      else checkUpperRightDiagonal state (cell-7) color
  else if ( (cell `div` 8) < 8 && (cell `div` 8) >= 0  && (cell `mod` 8) <= 7)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then do
            if (cell `mod` 8) /= 7 then checkUpperRightDiagonal state (cell-7) color
            else False
        else do
           if ((getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
    else False

checkLowerRightDiagonal ::  GameState -> Int -> PColor -> Bool
checkLowerRightDiagonal state cell color = do
  if (((getSquareType(getSquareAt state cell)) == King) && ((getSquareColor(getSquareAt state cell)) == colorCompliment color))
    then do
      if cell `mod` 8 == 7
        then False
      else checkLowerRightDiagonal state (cell+9) color
  else if ( (cell `div` 8) < 8 && (cell `div` 8) >= 0  && (cell `mod` 8) <= 7)
      then do
        if (getSquareColor (getSquareAt state cell) == ((colorCompliment color)))
          then False
        else if (getSquareColor (getSquareAt state cell) == NoColor)
          then do
            if (cell `mod` 8) /= 7 then checkLowerRightDiagonal state (cell + 9) color
            else False
        else do
           if ((getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen)
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
       if (((cell `mod` 8) - 2) >=0 && (cell+8) `div` 8 <=7)
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
