module GameUtils where


import Types
import BoardUtils
-- import MoveUtils
import Defaults
import Data.List

gameUtilsBoard :: Board
gameUtilsBoard = [
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
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

getCheckPositions :: GameState -> PColor ->Bool -> Int -> ([Int],[Int])
getCheckPositions state complimentColor firstIteration kingCell = ([(checkLeftColCheck state complimentColor firstIteration kingCell),(checkRightColCheck state complimentColor firstIteration kingCell)
     , (checkDownRowCheck state complimentColor firstIteration kingCell), (checkUpRowCheck state complimentColor firstIteration kingCell)
     , (checkUpperLeftDiagonal state complimentColor firstIteration kingCell) , (checkLowerLeftDiagonal state complimentColor firstIteration kingCell)
     , (checkUpperRightDiagonal state complimentColor firstIteration kingCell) ,(checkLowerRightDiagonal  state complimentColor firstIteration kingCell)],
     [ (checkUMidLeftHorseCheck state complimentColor  kingCell) , (checkLMidLeftHorseCheck state complimentColor  kingCell)
     , (checkUpperLeftHorseCheck state complimentColor  kingCell) , (checkLowerLeftHorseCheck state complimentColor  kingCell)
     , (checkUMidLeftHorseCheck state complimentColor  kingCell), (checkUpperRightHorseCheck state complimentColor  kingCell)
     , (checkLowerRightHorseCheck state complimentColor  kingCell) , (checkUMidRightHorseCheck state complimentColor  kingCell)
     , (checkLMidRightHorseCheck state complimentColor  kingCell)])

checkForGameCheck :: GameState -> PColor -> Bool -> Int -> Bool
checkForGameCheck state color firstIteration kingCell =
  let (l1,l2) = getCheckPositions state (colorCompliment color) firstIteration kingCell in
  let checkList = filter(\x -> x>= 0) (l1++l2) in
  if length(checkList) > 0 then True
  else False

canKingMove :: GameState -> PColor -> Int -> Bool
canKingMove state color cell =
  let r = cell `div` 8 in
  let c = cell `mod` 8 in
  let l = map getCellIndex [(rr,cc) | rr <- [r-1,r+1], cc <- [c-1,c+1], rr>=0 , cc>=0, rr<=7, cc<=7] in
  not (foldr (&&) True (map (checkForGameCheck state color False) l))

canAttackCheckPiece :: GameState -> PColor -> Int -> ([Int],[Int]) -> [Int] -> Bool
canAttackCheckPiece state color cell (list1,list2) checkList
  | (length(checkList) >= 2) = False
  | (length(checkList) == 1) =
    if (length(filter(\x -> x>= 0) (list2)) == 1) then canKillHorse state (colorCompliment color) (checkList !! 0)
    else canBlockCheckPiece state (colorCompliment color) cell (list1,list2) checkList

canKillHorse ::  GameState -> PColor -> Int -> Bool
canKillHorse state color cell = checkForGameCheck state color True cell

canBlockCheckPiece ::  GameState -> PColor -> Int -> ([Int],[Int]) -> [Int] -> Bool
canBlockCheckPiece state color cell (list1,list2) checkList =
  let index = elemIndex ((checkList !! 0)) list1 in
  let r = cell `div` 8 in
  let c = cell `div` 8 in
    if      (index == Just 0)  then checkLeftColHit           state color r ((checkList!!0) `mod` 8) c
    else if (index == Just 1)  then checkRightColHit          state color r ((checkList!!0) `mod` 8) c
    else if (index == Just 2)  then checkDownRowHit           state color r ((checkList!!0) `div` 8) c
    else if (index == Just 3)  then checkUpRowHit             state color r ((checkList!!0) `div` 8) c
    else if (index == Just 4)  then checkUpperLeftDiagHit     state color r ((checkList!!0)        ) c
    else if (index == Just 5)  then checkLowerLeftDiagHit     state color r ((checkList!!0)        ) c
    else if (index == Just 6)  then checkUpperRightDiagHit    state color r ((checkList!!0)        ) c
    else                       checkLowerRightDiagHit         state color r ((checkList!!0)        ) c


checkRightColHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkRightColHit state color startRow endCol startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow], cc <- [(startCol+1),(startCol+2)..endCol]] in
    not (foldr (||) False (map (checkForGameCheck state color False) l))

checkLeftColHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkLeftColHit state color startRow endCol startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow], cc <- [startCol-1,startCol-2..endCol]] in
    not (foldr (||) False (map (checkForGameCheck state color False) l))

checkUpRowHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkUpRowHit state color startRow endRow startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow-1,startRow-2..endRow], cc <- [startCol]] in
    not (foldr (||) False (map (checkForGameCheck state color False) l))

checkDownRowHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkDownRowHit state color startRow endRow startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow+1,startRow+2..endRow], cc <- [startCol]] in
    not (foldr (||) False (map (checkForGameCheck state color False) l))

checkUpperLeftDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkUpperLeftDiagHit state color startRow endCell startCol =
  let endRow = endCell `div` 8 in
  let endCol = endCell `mod` 8 in
  let l = map getCellIndex [(rr,cc) | rr <- [startRow-1,startRow-2..endRow], cc <- [startCol-1,startCol-2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
    not (foldr (||) False (map (checkForGameCheck state color False) l))

checkLowerLeftDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkLowerLeftDiagHit state color startRow endCell startCol =
  let endRow = endCell `div` 8 in
  let endCol = endCell `mod` 8 in
  let l = map getCellIndex [(rr,cc) | rr <- [startRow+1,startRow+2..endRow], cc <- [startCol-1,startCol-2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
    not (foldr (||) False (map (checkForGameCheck state color False) l))

checkUpperRightDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkUpperRightDiagHit state color startRow endCell startCol =
  let endRow = endCell `div` 8 in
  let endCol = endCell `mod` 8 in
  let l = map getCellIndex [(rr,cc) | rr <- [startRow-1,startRow-2..endRow], cc <- [startCol+1,startCol+2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
    not (foldr (||) False (map (checkForGameCheck state color False) l))

checkLowerRightDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkLowerRightDiagHit state color startRow endCell startCol =
  let endRow = endCell `div` 8 in
  let endCol = endCell `mod` 8 in
  let l = map getCellIndex [(rr,cc) | rr <- [startRow+1,startRow+2..endRow], cc <- [startCol+1,startCol+2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
    not (foldr (||) False (map (checkForGameCheck state color False) l))

getCellIndex :: (Int,Int) -> Int
getCellIndex (x,y) = x*8 + y

checkPawnCheck :: GameState -> PColor -> Int -> Int
checkPawnCheck state color cell
    | ((cell-7) `div` 8 >= 0 || (cell-9) `div` 8 >= 0) =
        if (getSquareColor (getSquareAt state (cell-7)) == color && (getSquareType (getSquareAt state  (cell-7))) == Pawn) then (cell-7)
        else if (getSquareColor (getSquareAt state (cell-9)) == color && (getSquareType (getSquareAt state  (cell-9))) == Pawn) then (cell-9)
        else -1
    | otherwise = -1

checkLeftColCheck :: GameState -> PColor -> Bool -> Int -> Int
checkLeftColCheck state color firstIteration cell
    | (getSquareType(getSquareAt state cell)) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 0 then -1
        else checkLeftColCheck state color False (cell-1)
    | (firstIteration ==True && cell `mod` 8 /= 0) = checkLeftColCheck state color False (cell-1)
    | (cell >= 0) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then -1
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 0 then checkLeftColCheck state color False (cell-1)
             else -1
        else
             if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen) then cell
             else -1
    | otherwise = -1

checkRightColCheck :: GameState -> PColor -> Bool -> Int -> Int
checkRightColCheck state color firstIteration cell
    | (getSquareType(getSquareAt state cell)) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 7 then -1
        else checkRightColCheck state color False (cell+1)
    | (firstIteration ==True && cell `mod` 8 /= 7) = checkRightColCheck state color False (cell+1)
    | (cell <= 63) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then -1
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 7 then checkRightColCheck state color False (cell+1)
             else -1
        else
             if ((getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen) then cell
             else -1
    | otherwise = -1

checkDownRowCheck ::  GameState -> PColor -> Bool -> Int -> Int
checkDownRowCheck state color firstIteration cell
    | (getSquareType(getSquareAt state cell)) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `div` 8 == 7 then -1
        else checkDownRowCheck state color False (cell+8)
    | (firstIteration ==True) = checkDownRowCheck state color False (cell+8)
    | (cell `div` 8) < 8 =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then -1
        else if getSquareColor (getSquareAt state cell) == NoColor then
             if (cell `div` 8) /= 7 then checkDownRowCheck state color False (cell+8)
             else -1
        else
             if (getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen then cell
             else -1
    | otherwise = -1

checkUpRowCheck ::  GameState -> PColor -> Bool -> Int -> Int
checkUpRowCheck state color firstIteration cell
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `div` 8 == 0 then -1
        else checkUpRowCheck state color False (cell-8)
    | (firstIteration ==True) = checkUpRowCheck state color False (cell-8)
    | ((cell `div` 8) < 8) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then -1
        else if getSquareColor (getSquareAt state cell) == NoColor then
             if (cell `div` 8) /= 0 then checkUpRowCheck state color False (cell-8)
             else -1
        else
             if (getSquareType (getSquareAt state cell)) == Rook || (getSquareType (getSquareAt state cell)) == Queen then cell
             else -1
    | otherwise = -1

checkUpperLeftDiagonal ::  GameState -> PColor -> Bool -> Int -> Int
checkUpperLeftDiagonal state color firstIteration cell
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 0 || cell `div` 8 == 0 then -1
        else checkUpperLeftDiagonal state color False (cell-9)
    | (firstIteration ==True && (cell `mod` 8) /= 0 && cell `div` 8 /= 0) = checkUpperLeftDiagonal state color False (cell-9)
    | ((cell `div` 8) < 8) && ((cell `div` 8) >= 0) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then -1
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 0 && cell `div` 8 /= 0 then checkUpperLeftDiagonal state color False (cell-9)
             else -1
        else
             if (getSquareType (getSquareAt state cell)) == Bishop || (getSquareType (getSquareAt state cell)) == Queen then cell
             else -1
    | otherwise = -1

checkLowerLeftDiagonal ::  GameState -> PColor -> Bool -> Int -> Int
checkLowerLeftDiagonal state color firstIteration cell
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 0 || cell `div` 8 == 7 then -1
        else checkLowerLeftDiagonal state color False (cell+7)
    | (firstIteration ==True && (cell `mod` 8) /= 0 && cell `div` 8 /= 7) = checkLowerLeftDiagonal state color False (cell+7)
    | ((cell `div` 8) < 8) && ((cell `div` 8) >= 0) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then -1
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 0 && cell `div` 8 /= 7 then checkLowerLeftDiagonal state color False (cell+7)
             else -1
        else
             if (getSquareType (getSquareAt state cell) == Bishop) || (getSquareType (getSquareAt state cell)) == Queen then cell
             else -1
    | otherwise = -1

checkUpperRightDiagonal ::  GameState -> PColor -> Bool -> Int -> Int
checkUpperRightDiagonal state color firstIteration cell
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 7 || cell `div` 8 == 0 then -1
        else checkUpperRightDiagonal state color False (cell-7)
    | (firstIteration ==True && (cell `mod` 8) /= 7 && cell `div` 8 /= 0) = checkUpperRightDiagonal state color False (cell-7)
    | ((cell `div` 8) < 8) && ((cell `div` 8) >= 0) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then -1
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 7 && cell `div` 8 /= 0 then checkUpperRightDiagonal state color False (cell-7)
             else -1
        else
             if (getSquareType (getSquareAt state cell) == Bishop) || (getSquareType (getSquareAt state cell)) == Queen then cell
             else -1
    | otherwise = -1

checkLowerRightDiagonal ::  GameState -> PColor -> Bool -> Int -> Int
checkLowerRightDiagonal state color firstIteration cell
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 7 || cell `div` 8 == 7 then -1
        else checkLowerRightDiagonal state color False (cell+9)
    | (firstIteration ==True && (cell `mod` 8) /= 7 && cell `div` 8 /= 7) = checkLowerRightDiagonal state color False (cell+9)
    | ((cell `div` 8) < 8) && ((cell `div` 8) >= 0) =
        if getSquareColor (getSquareAt state cell) == colorCompliment color then -1
        else if (getSquareColor (getSquareAt state cell) == NoColor) then
             if (cell `mod` 8) /= 7 && cell `div` 8 /= 7 then checkLowerRightDiagonal state color False (cell+9)
             else -1
        else
             if (getSquareType (getSquareAt state cell) == Bishop) || (getSquareType (getSquareAt state cell)) == Queen then cell
             else -1
    | otherwise = -1

checkUpperLeftHorseCheck :: GameState -> PColor -> Int -> Int
checkUpperLeftHorseCheck state color cell
    | (((cell-16) `div` 8)  >=0 && ((cell `mod` 8)-1) >= 0) =
           if getSquareColor (getSquareAt state (cell -17)) == color && (getSquareType (getSquareAt state (cell-17))) == Knight then (cell-17)
           else -1
    | otherwise = -1

checkUMidLeftHorseCheck :: GameState -> PColor -> Int -> Int
checkUMidLeftHorseCheck state color cell
    | (((cell-8) `div` 8) >=0 && ((cell `mod` 8)-2) >=0) =
           if getSquareColor (getSquareAt state (cell-10)) == color && getSquareType (getSquareAt state (cell-10)) == Knight then (cell-10)
           else -1
    | otherwise = -1

checkLMidLeftHorseCheck :: GameState -> PColor -> Int -> Int
checkLMidLeftHorseCheck state color cell
    | (((cell `mod` 8) - 2) >=0 && (cell+8) `div` 8 <=7) =
           if getSquareColor (getSquareAt state (cell+6)) == color && (getSquareType (getSquareAt state (cell+6))) == Knight then (cell+6)
           else -1
    | otherwise = -1

checkLowerLeftHorseCheck :: GameState -> PColor -> Int -> Int
checkLowerLeftHorseCheck state color cell
    | (((cell+16) `div` 8) <=7 && ((cell `mod` 8)-1) >=0) =
           if getSquareColor (getSquareAt state (cell+15)) == color && (getSquareType (getSquareAt state (cell+15))) == Knight then (cell+15)
           else -1
    | otherwise = -1

checkUpperRightHorseCheck ::  GameState -> PColor -> Int -> Int
checkUpperRightHorseCheck state color cell
    | ((cell-16) `div` 8 >=0 && ((cell `mod` 8)+1)<= 7) =
          if getSquareColor (getSquareAt state (cell -15)) == color && (getSquareType (getSquareAt state (cell-15))) == Knight then (cell-15)
          else -1
    | otherwise = -1

checkUMidRightHorseCheck ::  GameState -> PColor -> Int -> Int
checkUMidRightHorseCheck state color cell
    | ((cell-8) `div` 8 >=0 && ((cell `mod` 8)+2) <= 7) =
           if getSquareColor (getSquareAt state (cell-6)) == color && (getSquareType (getSquareAt state (cell-6))) == Knight then (cell-6)
           else -1
    | otherwise = -1

checkLMidRightHorseCheck ::  GameState -> PColor -> Int -> Int
checkLMidRightHorseCheck state color cell
       | (((cell `mod` 8)+2) >=0 && (cell+8) `div` 8 <=7) =
              if getSquareColor (getSquareAt state (cell+10)) == color && (getSquareType (getSquareAt state (cell+10))) == Knight then (cell+10)
              else -1
       | otherwise = -1

checkLowerRightHorseCheck :: GameState -> PColor -> Int -> Int
checkLowerRightHorseCheck state color cell
    | ((cell+16) `div` 8 <=7 && ((cell `mod` 8)+1) <=7) =
            if getSquareColor (getSquareAt state (cell+17)) == color && (getSquareType (getSquareAt state (cell+17))) == Knight then (cell+17)
            else -1
     | otherwise = -1
