module GameUtils where


import Types
import BoardUtils
import Defaults
import Data.List

colorCompliment :: PColor -> PColor --Function which returns the opposite colour.
colorCompliment color
      | color == White = Black
      | color == Black = White
      | otherwise = NoColor         -- Return NoColor if square is empty.

getCheckPositions :: GameState -> PColor ->Bool -> Int -> ([Int],[Int]) --This function will compute a tuple of lists which will cimpute all check positions.
getCheckPositions state complimentColor firstIteration kingCell = (
        
        -- all non horse check positions
         map (\x -> x state complimentColor firstIteration kingCell)
            [ (checkLeftColCheck),
              (checkRightColCheck),
              (checkDownRowCheck),
              (checkUpRowCheck),
              (checkUpperLeftDiagonal),
              (checkLowerLeftDiagonal),
              (checkUpperRightDiagonal),
              (checkLowerRightDiagonal)
            ]
        ++ map (\x -> x state complimentColor kingCell)
            [(checkPawnCheck7), (checkPawnCheck9)],
        
        -- all 8 horse check positions
        map (\x -> x state complimentColor  kingCell)
            [ (checkUMidLeftHorseCheck),
              (checkLMidLeftHorseCheck),
              (checkUpperLeftHorseCheck),
              (checkLowerLeftHorseCheck),
              (checkUpperRightHorseCheck),
              (checkLowerRightHorseCheck),
              (checkUMidRightHorseCheck),
              (checkLMidRightHorseCheck)
            ]
     )
-- to know if its a check and mate      (check,mate)
getGameStatus :: GameState -> PColor -> (Bool,Bool)
getGameStatus state color =
    (\kingCell ->
        (\(l1,l2) ->
            (\checkList ->
                (
                    -- for check
                    length(checkList)>0,
                    -- for mate 
                    (length(checkList)>0) && (not (canKingMove state color kingCell)) && (not $ canAttackCheckPiece state color kingCell (l1,l2) checkList)
                )
            ) $ filter (\x -> x>= 0) (l1++l2)
        ) $ getCheckPositions state (colorCompliment color) False kingCell
    ) $ getKingPos state color

isInCheck :: GameState -> PColor -> Bool  --Function to check whether it is a check.
isInCheck state color = checkForGameCheck state color False (getKingPos state color)

--Function to compute list of all check positions.
checkForGameCheck :: GameState -> PColor -> Bool -> Int -> Bool 
checkForGameCheck state color firstIteration kingCell =
  let (l1,l2) = getCheckPositions state (colorCompliment color) firstIteration kingCell in
  let checkList = filter(\x -> x>= 0) (l1++l2) in
  (length(checkList) > 0)

canKingMove :: GameState -> PColor -> Int -> Bool --To know whether the king can move.
canKingMove state color cell =
  let r = cell `div` 8 in
  let c = cell `mod` 8 in
  let l = filter (\x -> (getSquareColorAt state x) /= color) $ map getCellIndex [(rr,cc) | rr <- [r-1,r,r+1], cc <- [c-1,c,c+1], rr>=0 , cc>=0, rr<=7, cc<=7, (rr,cc)/=(r,c)] in
  not (foldr (&&) True (map (checkForGameCheck state color True) l))

canAttackCheckPiece :: GameState -> PColor -> Int -> ([Int],[Int]) -> [Int] -> Bool --Try to block or kill the attacking piece.
canAttackCheckPiece state color cell (list1,list2) checkList
  | (length(checkList) >= 2) = False --If there is more than one check then it is a check.
  | (length(checkList) == 1) =       --If only one check then try to block or attack.
    if (length(filter(\x -> x>= 0) (list2)) == 1) then canKillHorse state (colorCompliment color) (checkList !! 0) --If it is a horse then we should kill it.
    else canBlockCheckPiece state (colorCompliment color) cell (list1,list2) checkList --Else kill or block the piece.
canAttackCheckPiece _ _ _ _ _ = True

-- if we can kill the horse at given position
canKillHorse ::  GameState -> PColor -> Int -> Bool
canKillHorse state color cell = checkForGameCheck state color True cell

canBlockCheckPiece ::  GameState -> PColor -> Int -> ([Int],[Int]) -> [Int] -> Bool
canBlockCheckPiece state color cell (list1,list2) checkList --Find the path to block the check piece.
    | (index == Just 0)  = checkLeftColHit        state color r (checkListHead `mod` 8) c
    | (index == Just 1)  = checkRightColHit       state color r (checkListHead `mod` 8) c
    | (index == Just 2)  = checkDownRowHit        state color r (checkListHead `div` 8) c
    | (index == Just 3)  = checkUpRowHit          state color r (checkListHead `div` 8) c
    | (index == Just 4)  = checkUpperLeftDiagHit  state color r (checkListHead        ) c
    | (index == Just 5)  = checkLowerLeftDiagHit  state color r (checkListHead        ) c
    | (index == Just 6)  = checkUpperRightDiagHit state color r (checkListHead        ) c
    | (index == Just 7)  = checkLowerRightDiagHit state color r (checkListHead        ) c
    | otherwise          = checkPawnHit           state color checkListHead
    where
        checkListHead = checkList!!0
        index = elemIndex (checkListHead) list1
        r = cell `div` 8
        c = cell `mod` 8

{----------------- Booleans if it can Hit ---------------------}
checkPawnHit :: GameState -> PColor -> Int -> Bool
checkPawnHit state color cell = checkForGameCheck state color False cell

--Check pieces to the right
checkRightColHit :: GameState -> PColor -> Int -> Int -> Int -> Bool  
checkRightColHit state color startRow endCol startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow], cc <- [(startCol+1),(startCol+2)..endCol]] in
    foldr (||) False (map (checkForGameCheck state color False) l)

--Check pieces to the left
checkLeftColHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkLeftColHit state color startRow endCol startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow], cc <- [startCol-1,startCol-2..endCol]] in
    foldr (||) False (map (checkForGameCheck state color False) l)

--Check pieces to the top
checkUpRowHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkUpRowHit state color startRow endRow startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow-1,startRow-2..endRow], cc <- [startCol]] in
    foldr (||) False (map (checkForGameCheck state color False) l)

--Check pieces to the bottom
checkDownRowHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkDownRowHit state color startRow endRow startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow+1,startRow+2..endRow], cc <- [startCol]] in
    foldr (||) False (map (checkForGameCheck state color False) l)

--Check pieces to the top left
checkUpperLeftDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkUpperLeftDiagHit state color startRow endCell startCol =
    (\endRow endCol ->
      let l = map getCellIndex [(rr,cc) | rr <- [startRow-1,startRow-2..endRow], cc <- [startCol-1,startCol-2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
        foldr (||) False (map (checkForGameCheck state color False) l)
    ) (endCell `div` 8) (endCell `mod` 8)

--Check pieces to the bottom left
checkLowerLeftDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkLowerLeftDiagHit state color startRow endCell startCol =
    (\endRow endCol ->
      let l = map getCellIndex [(rr,cc) | rr <- [startRow+1,startRow+2..endRow], cc <- [startCol-1,startCol-2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
        foldr (||) False (map (checkForGameCheck state color False) l)
    ) (endCell `div` 8) (endCell `mod` 8)

--Check pieces to the top right
checkUpperRightDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkUpperRightDiagHit state color startRow endCell startCol =
    (\endRow endCol ->
      let l = map getCellIndex [(rr,cc) | rr <- [startRow-1,startRow-2..endRow], cc <- [startCol+1,startCol+2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
        foldr (||) False (map (checkForGameCheck state color False) l)
    ) (endCell `div` 8) (endCell `mod` 8)

--Check pieces to the bottom right
checkLowerRightDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkLowerRightDiagHit state color startRow endCell startCol =
    (\endRow endCol ->
      let l = map getCellIndex [(rr,cc) | rr <- [startRow+1,startRow+2..endRow], cc <- [startCol+1,startCol+2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
        foldr (||) False (map (checkForGameCheck state color False) l)
    ) (endCell `div` 8) (endCell `mod` 8)
{----------------- /Booleans if it can Hit ---------------------}

getCellIndex :: (Int,Int) -> Int --To find the cell from row and column.
getCellIndex (x,y) = x*8 + y

{----------------- Position where it can Hit ---------------------}

checkPawnCheck7 :: GameState -> PColor -> Int -> Int
checkPawnCheck7 state color cell
    | checkTopDownHit
        && (((cell-7) >= 0) && (getSquareAt state (cell-7)) == (Piece color Pawn)) && (getSquareAt state (cell))/=Empty
        = cell-7
    | (not checkTopDownHit)
        && (((cell+7) <= 63) && (getSquareAt state (cell+7)) == (Piece color Pawn)) && (getSquareAt state (cell))/=Empty
        = cell+7
    | otherwise = -1
    where
        mycolor = if (getTurn state)==PlayerW then White else Black
        checkTopDownHit = (mycolor/=color)

checkPawnCheck9 :: GameState -> PColor -> Int -> Int
checkPawnCheck9 state color cell
    | checkTopDownHit
        && (((cell-9) >= 0) && (getSquareAt state (cell-9)) == (Piece color Pawn)) && (getSquareAt state (cell))/=Empty
        = cell-9
    | (not checkTopDownHit)
        && (((cell+9) <= 63) && (getSquareAt state (cell+9)) == (Piece color Pawn)) && (getSquareAt state (cell))/=Empty
        = cell+9
    | otherwise = -1
    where
        mycolor = if (getTurn state)==PlayerW then White else Black
        checkTopDownHit = (mycolor/=color)

--All the functions below check for check in one particular direction wiht respect to he king.
--All the functions will have a limit like edge cases when we reach the end of a row or a column.
--First iteration is used for canKillHorse in checkmate.
--Since we we are passing the kings position we can skip that particular index.
--If a particular color of a piece is of the oppsite color then it is a check or if it is aof the same color then that path is safe.
--If the square is empty then we recurse through the path.

checkLeftColCheck :: GameState -> PColor -> Bool -> Int -> Int
checkLeftColCheck state color firstIteration cell
    | cell<0 || cell>63 = -1
    | (getSquareType(getSquareAt state cell)) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 0 then -1
        else checkLeftColCheck state color False (cell-1)
    | (firstIteration && cell `mod` 8 /= 0) = checkLeftColCheck state color False (cell-1)
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
    | cell<0 || cell>63 = -1
    | (getSquareType(getSquareAt state cell)) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 7 then -1
        else checkRightColCheck state color False (cell+1)
    | (firstIteration && cell `mod` 8 /= 7) = checkRightColCheck state color False (cell+1)
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
    | cell<0 || cell>63 = -1
    | (getSquareType(getSquareAt state cell)) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `div` 8 == 7 then -1
        else checkDownRowCheck state color False (cell+8)
    | (firstIteration) = checkDownRowCheck state color False (cell+8)
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
    | cell<0 || cell>63 = -1
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `div` 8 == 0 then -1
        else checkUpRowCheck state color False (cell-8)
    | (firstIteration) = checkUpRowCheck state color False (cell-8)
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
    | cell<0 || cell>63 = -1
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 0 || cell `div` 8 == 0 then -1
        else checkUpperLeftDiagonal state color False (cell-9)
    | (firstIteration && (cell `mod` 8) /= 0 && cell `div` 8 /= 0) = checkUpperLeftDiagonal state color False (cell-9)
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
    | cell<0 || cell>63 = -1
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 0 || cell `div` 8 == 7 then -1
        else checkLowerLeftDiagonal state color False (cell+7)
    | (firstIteration && (cell `mod` 8) /= 0 && cell `div` 8 /= 7) = checkLowerLeftDiagonal state color False (cell+7)
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
    | cell<0 || cell>63 = -1
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 7 || cell `div` 8 == 0 then -1
        else checkUpperRightDiagonal state color False (cell-7)
    | (firstIteration && (cell `mod` 8) /= 7 && cell `div` 8 /= 0) = checkUpperRightDiagonal state color False (cell-7)
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
    | cell<0 || cell>63 = -1
    |  getSquareType(getSquareAt state cell) == King && getSquareColor(getSquareAt state cell) == colorCompliment color =
        if cell `mod` 8 == 7 || cell `div` 8 == 7 then -1
        else checkLowerRightDiagonal state color False (cell+9)
    | (firstIteration && (cell `mod` 8) /= 7 && cell `div` 8 /= 7) = checkLowerRightDiagonal state color False (cell+9)
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
    | cell<0 || cell>63 = -1
    | (((cell-16) `div` 8)  >=0 && ((cell `mod` 8)-1) >= 0) =
           if getSquareColor (getSquareAt state (cell -17)) == color && (getSquareType (getSquareAt state (cell-17))) == Knight then (cell-17)
           else -1
    | otherwise = -1

checkUMidLeftHorseCheck :: GameState -> PColor -> Int -> Int
checkUMidLeftHorseCheck state color cell
    | cell<0 || cell>63 = -1
    | (((cell-8) `div` 8) >=0 && ((cell `mod` 8)-2) >=0) =
           if getSquareColor (getSquareAt state (cell-10)) == color && getSquareType (getSquareAt state (cell-10)) == Knight then (cell-10)
           else -1
    | otherwise = -1

checkLMidLeftHorseCheck :: GameState -> PColor -> Int -> Int
checkLMidLeftHorseCheck state color cell
    | cell<0 || cell>63 = -1
    | (((cell `mod` 8) - 2) >=0 && (cell+8) `div` 8 <=7) =
           if getSquareColor (getSquareAt state (cell+6)) == color && (getSquareType (getSquareAt state (cell+6))) == Knight then (cell+6)
           else -1
    | otherwise = -1

checkLowerLeftHorseCheck :: GameState -> PColor -> Int -> Int
checkLowerLeftHorseCheck state color cell
    | cell<0 || cell>63 = -1
    | (((cell+16) `div` 8) <=7 && ((cell `mod` 8)-1) >=0) =
           if getSquareColor (getSquareAt state (cell+15)) == color && (getSquareType (getSquareAt state (cell+15))) == Knight then (cell+15)
           else -1
    | otherwise = -1

checkUpperRightHorseCheck ::  GameState -> PColor -> Int -> Int
checkUpperRightHorseCheck state color cell
    | cell<0 || cell>63 = -1
    | ((cell-16) `div` 8 >=0 && ((cell `mod` 8)+1)<= 7) =
          if getSquareColor (getSquareAt state (cell -15)) == color && (getSquareType (getSquareAt state (cell-15))) == Knight then (cell-15)
          else -1
    | otherwise = -1

checkUMidRightHorseCheck ::  GameState -> PColor -> Int -> Int
checkUMidRightHorseCheck state color cell
    | cell<0 || cell>63 = -1
    | ((cell-8) `div` 8 >=0 && ((cell `mod` 8)+2) <= 7) =
           if getSquareColor (getSquareAt state (cell-6)) == color && (getSquareType (getSquareAt state (cell-6))) == Knight then (cell-6)
           else -1
    | otherwise = -1

checkLMidRightHorseCheck ::  GameState -> PColor -> Int -> Int
checkLMidRightHorseCheck state color cell
    | cell<0 || cell>63 = -1
    | (((cell `mod` 8)+2) >=0 && (cell+8) `div` 8 <=7) =
            if getSquareColor (getSquareAt state (cell+10)) == color && (getSquareType (getSquareAt state (cell+10))) == Knight then (cell+10)
            else -1
    | otherwise = -1

checkLowerRightHorseCheck :: GameState -> PColor -> Int -> Int
checkLowerRightHorseCheck state color cell
    | cell<0 || cell>63 = -1
    | ((cell+16) `div` 8 <=7 && ((cell `mod` 8)+1) <=7) =
            if getSquareColor (getSquareAt state (cell+17)) == color && (getSquareType (getSquareAt state (cell+17))) == Knight then (cell+17)
            else -1
    | otherwise = -1
{----------------- / Position where it can Hit ---------------------}
