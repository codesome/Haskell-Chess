module GameUtils where


import Types
import BoardUtils
-- import MoveUtils
import Defaults


gameUtilsBoard :: Board
gameUtilsBoard = [
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   bKnight,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   wKing,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty]
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
        boardPoints=initialDisplayPointsW, moveEnabled=True
}

colorCompliment :: PColor -> PColor
colorCompliment color1 = color2 where
  color2
      | color1 == White = Black
      | color1 == Black = White

checkForGameCheck :: GameState -> Int -> PColor -> Bool
checkForGameCheck state kingCell color =
  let complimentColor = colorCompliment color in
  if ((checkLeftColCheck state (kingCell) complimentColor)||(checkRightColCheck state (kingCell) complimentColor)
       || (checkDownRowCheck state (kingCell) complimentColor)|| (checkUpRowCheck state (kingCell) complimentColor)
       || (checkUpperLeftDiagonal state (kingCell) complimentColor) || (checkLowerLeftDiagonal state (kingCell) complimentColor)
       || (checkLowerRightDiagonal state (kingCell) complimentColor) || (checkUpperRightDiagonal state (kingCell) complimentColor)
       || (checkUMidLeftHorseCheck state kingCell complimentColor) || (checkLMidLeftHorseCheck state kingCell complimentColor)
       || (checkUpperLeftHorseCheck state kingCell complimentColor) || (checkLowerLeftHorseCheck state kingCell complimentColor)
       || (checkUMidLeftHorseCheck state kingCell complimentColor)|| (checkUpperRightHorseCheck state kingCell complimentColor)
       || (checkLowerRightHorseCheck state kingCell complimentColor) || (checkUMidRightHorseCheck state kingCell complimentColor)
       || (checkLMidRightHorseCheck state kingCell complimentColor)
    )
      then True
  else False

checkForPawn :: GameState -> Int -> PColor -> Bool
checkForPawn state cell color
  | ((cell-7) `div` 8 >= 0 || (cell-9) `div` 8 >= 0) =
      if (getSquareColor (getSquareAt state (cell-7)) == color && (getSquareType (getSquareAt state  (cell-7))) == Pawn) then True
      else if (getSquareColor (getSquareAt state (cell-9)) == color && (getSquareType (getSquareAt state  (cell-9))) == Pawn) then True
      else False
  | otherwise = False

checkLeftColCheck :: GameState -> Int -> PColor -> Bool
checkLeftColCheck state cell color
    | (getSquareType(getSquareAt state cell)) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 0 then False
        else checkLeftColCheck state (cell-1) color
    | (cell >= 0) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then False
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 0 then checkLeftColCheck state (cell-1) color
             else False
        else
             if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen) then True
             else False
    | otherwise = False

checkRightColCheck :: GameState -> Int -> PColor -> Bool
checkRightColCheck state cell color
    | (getSquareType(getSquareAt state cell)) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 7 then False
        else checkRightColCheck state (cell+1) color
    | (cell <= 63) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then False
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 7 then checkRightColCheck state (cell+1) color
             else False
        else
             if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen) then True
             else False
    | otherwise = False

checkDownRowCheck ::  GameState -> Int -> PColor -> Bool
checkDownRowCheck state cell color
    | (getSquareType(getSquareAt state cell)) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `div` 8 == 7 then False
        else checkDownRowCheck state (cell+8) color
    | (cell `div` 8) < 8 =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then False
        else if getSquareColor (getSquareAt state cell) == NoColor then
             if (cell `div` 8) /= 7 then checkDownRowCheck state (cell+8) color
             else False
        else
             if (getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen then True
             else False
    | otherwise = False

checkUpRowCheck ::  GameState -> Int -> PColor -> Bool
checkUpRowCheck state cell color
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `div` 8 == 0 then False
        else checkUpRowCheck state (cell-8) color
    | ((cell `div` 8) < 8) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then False
        else if getSquareColor (getSquareAt state cell) == NoColor then
             if (cell `div` 8) /= 0 then checkUpRowCheck state (cell-8) color
             else False
        else
             if (getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen then True
             else False
    | otherwise = False

checkUpperLeftDiagonal ::  GameState -> Int -> PColor -> Bool
checkUpperLeftDiagonal state cell color
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 0 || cell `div` 8 == 0 then False
        else checkUpperLeftDiagonal state (cell-9) color
    | ((cell `div` 8) < 8) && ((cell `div` 8) >= 0) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then False
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 0 && cell `div` 8 /= 0 then checkUpperLeftDiagonal state (cell-9) color
             else False
        else
             if (getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen then True
             else False
    | otherwise = False

checkLowerLeftDiagonal ::  GameState -> Int -> PColor -> Bool
checkLowerLeftDiagonal state cell color
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 0 || cell `div` 8 == 7 then False
        else checkLowerLeftDiagonal state (cell+7) color
    | ((cell `div` 8) < 8) && ((cell `div` 8) >= 0) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then False
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 0 && cell `div` 8 /= 7 then checkLowerLeftDiagonal state (cell+7) color
             else False
        else
             if (getSquareType (getSquareAt state cell) == Bishop) || (getSquareType (getSquareAt state cell)) == Queen then True
             else False
    | otherwise = False

checkUpperRightDiagonal ::  GameState -> Int -> PColor -> Bool
checkUpperRightDiagonal state cell color
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 7 || cell `div` 8 == 0 then False
        else checkUpperRightDiagonal state (cell-7) color
    | ((cell `div` 8) < 8) && ((cell `div` 8) >= 0) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then False
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 7 && cell `div` 8 /= 0 then checkUpperRightDiagonal state (cell-7) color
             else False
        else
             if (getSquareType (getSquareAt state cell) == Bishop) || (getSquareType (getSquareAt state cell)) == Queen then True
             else False
    | otherwise = False

checkLowerRightDiagonal ::  GameState -> Int -> PColor -> Bool
checkLowerRightDiagonal state cell color
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 7 || cell `div` 8 == 7 then False
        else checkLowerRightDiagonal state (cell+9) color
    | ((cell `div` 8) < 8) && ((cell `div` 8) >= 0) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then False
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 7 && cell `div` 8 /= 7 then checkLowerRightDiagonal state (cell+9) color
             else False
        else
             if (getSquareType (getSquareAt state cell) == Bishop) || (getSquareType (getSquareAt state cell)) == Queen then True
             else False
    | otherwise = False

checkUpperLeftHorseCheck :: GameState -> Int -> PColor -> Bool
checkUpperLeftHorseCheck state cell color
    | (((cell-16) `div` 8)  >=0 && ((cell `mod` 8)-1) >= 0) =
           if getSquareColor (getSquareAt state (cell -17)) == color && (getSquareType (getSquareAt state (cell-17))) == Knight then True
           else False
    | otherwise = False

checkUMidLeftHorseCheck :: GameState -> Int -> PColor -> Bool
checkUMidLeftHorseCheck state cell color
    | (((cell-8) `div` 8) >=0 && ((cell `mod` 8)-2) >=0) =
           if getSquareColor (getSquareAt state (cell-10)) == color && getSquareType (getSquareAt state (cell-10)) == Knight then True
           else False
    | otherwise = False

checkLMidLeftHorseCheck :: GameState -> Int -> PColor -> Bool
checkLMidLeftHorseCheck state cell color
    | (((cell `mod` 8) - 2) >=0 && (cell+8) `div` 8 <=7) =
           if getSquareColor (getSquareAt state (cell+6)) == color && (getSquareType (getSquareAt state (cell+6))) == Knight then True
           else False
    | otherwise = False

checkLowerLeftHorseCheck :: GameState -> Int -> PColor -> Bool
checkLowerLeftHorseCheck state cell color
    | (((cell+16) `div` 8) <=7 && ((cell `mod` 8)-1) >=0) =
           if getSquareColor (getSquareAt state (cell+15)) == color && (getSquareType (getSquareAt state (cell+15))) == Knight then True
           else False
    | otherwise = False

checkUpperRightHorseCheck ::  GameState -> Int -> PColor -> Bool
checkUpperRightHorseCheck state cell color
    | ((cell-16) `div` 8 >=0 && ((cell `mod` 8)+1)<= 7) =
          if getSquareColor (getSquareAt state (cell -15)) == color && (getSquareType (getSquareAt state (cell-15))) == Knight then True
          else False
    | otherwise = False

checkUMidRightHorseCheck ::  GameState -> Int -> PColor -> Bool
checkUMidRightHorseCheck state cell color
    | ((cell-8) `div` 8 >=0 && ((cell `mod` 8)+2) <= 7) =
           if getSquareColor (getSquareAt state (cell-6)) == color && (getSquareType (getSquareAt state (cell-6))) == Knight then True
           else False
    | otherwise = False

checkLMidRightHorseCheck ::  GameState -> Int -> PColor -> Bool
checkLMidRightHorseCheck state cell color
       | (((cell `mod` 8)+2) >=0 && (cell+8) `div` 8 <=7) =
              if getSquareColor (getSquareAt state (cell+10)) == color && (getSquareType (getSquareAt state (cell+10))) == Knight then True
              else False
       | otherwise = False

checkLowerRightHorseCheck :: GameState -> Int -> PColor -> Bool
checkLowerRightHorseCheck state cell color
    | ((cell+16) `div` 8 <=7 && ((cell `mod` 8)+1) <=7) =
            if getSquareColor (getSquareAt state (cell+17)) == color && (getSquareType (getSquareAt state (cell+17))) == Knight then True
            else False
     | otherwise = False
