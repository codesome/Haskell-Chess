module GameUtils where


import Types
import BoardUtils
import DisplayUtils
import MoveUtils
import Defaults


checkForGameCheck :: GameState -> Int -> Bool
checkForGameCheck state kingCell = do
    if ((checkLeftColCheck state (kingCell-1))||(checkRightColCheck state (kingCell+1))||(checkDownRowCheck state (kingCell+8))||
      (checkUpRowCheck state (kingCell-8)) || (checkUpperLeftDiagonal state (kingCell-9)) || (checkLowerLeftDiagonal state (kingCell+7))||
      (checkLowerRightDiagonal state (kingCell+9)) || (checkUpperRightDiagonal state (kingCell-7))
      || (checkUMidLeftHorseCheck state kingCell) || (checkLMidLeftHorseCheck state kingCell)
      || (checkUpperLeftHorseCheck state kingCell) || (checkLowerLeftHorseCheck state kingCell)
      || (checkUMidLeftHorseCheck state kingCell)|| (checkUpperRightHorseCheck state kingCell)
      || (checkLowerRightHorseCheck state kingCell) || (checkUMidRightHorseCheck state kingCell)
      || (checkLMidRightHorseCheck state kingCell))
      then True
    else False

checkLeftColCheck :: GameState -> Int -> Bool
checkLeftColCheck state cell = do
    if ((cell `mod` 8) > 0)
      then do
        if (getSquareColor (getSquareAt state cell) /= Black)
          then checkLeftColCheck state (cell-1)
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
     else if ((cell `mod` 8) == 0)
       then do
         if (getSquareColor (getSquareAt state cell) /= Black)
           then False
          else do
            if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
              then True
            else False
       else False


checkRightColCheck :: GameState -> Int -> Bool
checkRightColCheck state cell = do
    if ((cell `mod` 8) < 7)
      then do
        if (getSquareColor (getSquareAt state cell) /= Black)
          then checkRightColCheck state (cell+1)
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
     else if ((cell `mod` 8) == 7)
       then do
         if (getSquareColor (getSquareAt state cell) /= Black)
           then False
          else do
            if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
              then True
            else False
      else False

checkDownRowCheck :: GameState -> Int -> Bool
checkDownRowCheck state cell = do
    if ((cell `div` 8) < 7)
      then do
        if (getSquareColor (getSquareAt state cell) /= Black)
          then checkDownRowCheck state (cell+8)
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
     else if ((cell `div` 8) == 7)
       then do
         if (getSquareColor (getSquareAt state cell) /= Black)
           then False
          else do
            if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
              then True
            else False
       else False

checkUpRowCheck :: GameState -> Int -> Bool
checkUpRowCheck state cell = do
    if ((cell `div` 8) > 0)
      then do
        if (getSquareColor (getSquareAt state cell) /= Black)
          then checkUpRowCheck state (cell-8)
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
     else if ((cell `div` 8) == 0)
       then do
         if (getSquareColor (getSquareAt state cell) /= Black)
           then False
          else do
            if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
              then True
            else False
       else False

checkUpperLeftDiagonal :: GameState -> Int -> Bool
checkUpperLeftDiagonal state cell = do
    if ((cell `mod` 8) > 0)
      then do
        if (getSquareColor (getSquareAt state cell) /= Black)
          then checkUpperLeftDiagonal state (cell-9)
        else do
           if ((getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen || (getSquareType (getSquareAt state cell)) == Pawn)
             then True
           else False
     else if ((cell `mod` 8) == 0)
       then do
         if (getSquareColor (getSquareAt state cell) /= Black)
           then False
          else do
            if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen || (getSquareType (getSquareAt state cell)) == Pawn)
              then True
            else False
       else False

checkLowerLeftDiagonal :: GameState -> Int -> Bool
checkLowerLeftDiagonal state cell = do
    if ((cell `mod` 8) > 0)
      then do
        if (getSquareColor (getSquareAt state cell) /= Black)
          then checkLowerLeftDiagonal state (cell+7)
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
     else if ((cell `div` 8) == 7)
       then do
         if (getSquareColor (getSquareAt state cell) /= Black)
           then False
          else do
            if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
              then True
            else False
       else False

checkUpperRightDiagonal :: GameState -> Int -> Bool
checkUpperRightDiagonal state cell = do
    if ((cell `mod` 8) < 7)
      then do
        if (getSquareColor (getSquareAt state cell) /= Black)
          then checkUpperRightDiagonal state (cell-7)
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen || (getSquareType (getSquareAt state cell)) == Pawn)
             then True
           else False
     else if ((cell `mod` 8) == 7)
       then do
         if (getSquareColor (getSquareAt state cell) /= Black)
           then False
          else do
            if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen || (getSquareType (getSquareAt state cell)) == Pawn)
              then True
            else False
     else False

checkLowerRightDiagonal :: GameState -> Int -> Bool
checkLowerRightDiagonal state cell = do
    if ((cell `div` 8) < 7)
      then do
        if (getSquareColor (getSquareAt state cell) /= Black)
          then checkUpperRightDiagonal state (cell+9)
        else do
           if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
             then True
           else False
     else if ((cell `div` 8) == 7)
       then do
         if (getSquareColor (getSquareAt state cell) /= Black)
           then False
          else do
            if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen)
              then True
            else False
       else False

checkUpperLeftHorseCheck :: GameState -> Int ->Bool
checkUpperLeftHorseCheck state cell = do
    if (((cell-16) `div` 8)  >=0 && ((cell `mod` 8)-1) >= 0)
      then do
        if (getSquareColor (getSquareAt state (cell -17)) == Black && (getSquareType (getSquareAt state (cell-17))) == Knight)
          then True
        else False
    else False

checkUMidLeftHorseCheck :: GameState -> Int ->Bool
checkUMidLeftHorseCheck state cell = do
    if ((cell-8) `div` 8 >=0 && ((cell `mod` 8)-2) >=0)
        then do
          if (getSquareColor (getSquareAt state (cell-10)) == Black && (getSquareType (getSquareAt state (cell-10))) == Knight)
            then True
          else False
     else False

checkLMidLeftHorseCheck :: GameState -> Int ->Bool
checkLMidLeftHorseCheck state cell = do
       if (((cell `mod` 8)-2) >=0 && (cell+8) `div` 8 <=7)
           then do
             if (getSquareColor (getSquareAt state (cell+6)) == Black && (getSquareType (getSquareAt state (cell+6))) == Knight)
               then True
             else False
        else False

checkLowerLeftHorseCheck :: GameState -> Int -> Bool
checkLowerLeftHorseCheck state cell = do
    if (((cell+16) `div` 8) <=7 && ((cell `mod` 8)-1) >=0)
        then do
          if (getSquareColor (getSquareAt state (cell+15)) == Black && (getSquareType (getSquareAt state (cell+15))) == Knight)
            then True
          else False
     else False

checkUpperRightHorseCheck :: GameState -> Int ->Bool
checkUpperRightHorseCheck state cell = do
    if ((cell-16) `div` 8 >=0 && ((cell `mod` 8)+1)<= 7)
      then do
        if (getSquareColor (getSquareAt state (cell -15)) == Black && (getSquareType (getSquareAt state (cell-15))) == Knight)
          then True
        else False
    else False

checkUMidRightHorseCheck :: GameState -> Int ->Bool
checkUMidRightHorseCheck state cell = do
    if ((cell-8) `div` 8 >=0 && ((cell `mod` 8)+2) <= 7)
        then do
          if (getSquareColor (getSquareAt state (cell-6)) == Black && (getSquareType (getSquareAt state (cell-6))) == Knight)
            then True
          else False
     else False

checkLMidRightHorseCheck :: GameState -> Int ->Bool
checkLMidRightHorseCheck state cell = do
       if (((cell `mod` 8)+2) >=0 && (cell+8) `div` 8 <=7)
           then do
             if (getSquareColor (getSquareAt state (cell+10)) == Black && (getSquareType (getSquareAt state (cell+10))) == Knight)
               then True
             else False
        else False

checkLowerRightHorseCheck :: GameState -> Int ->Bool
checkLowerRightHorseCheck state cell = do
    if ((cell+16) `div` 8 <=7 && ((cell `mod` 8)+1) <=7)
        then do
          if (getSquareColor (getSquareAt state (cell+17)) == Black && (getSquareType (getSquareAt state (cell+7))) == Knight)
            then True
          else False
     else False
