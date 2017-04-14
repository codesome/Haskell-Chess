module GameUtils where


import Types
import BoardUtils
import Defaults
import Data.List

gameUtilsBoard :: Board
gameUtilsBoard = [
        [Empty,   Empty,   Empty,   bRook,   Empty,   bRook,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   bRook],
        [Empty,   Empty,   Empty,   Empty,   wKing,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   bRook],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty],
        [Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty,   Empty]
    ]

checkGameState :: GameState
checkGameState = GameState {
        board = gameUtilsBoard,
        turn = PlayerW,
        whiteKing=36,
        blackKing=4,
        startPointIsSet=False,
        startPoint=0,
        boardPoints=initialDisplayPointsW, moveEnabled=True
}

colorCompliment :: PColor -> PColor
colorCompliment color 
      | color == White = Black
      | color == Black = White
      | otherwise = NoColor

getCheckPositions :: GameState -> PColor ->Bool -> Int -> ([Int],[Int])
getCheckPositions state complimentColor firstIteration kingCell = (
        -- all non horse
         map (\x -> if x==kingCell then -1 else x) $ 
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
        -- all 8 horse
        map (\x -> if x==kingCell then -1 else x) $ map (\x -> x state complimentColor  kingCell)
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
                                     -- (check,mate)
getGameStatus :: GameState -> PColor -> (Bool,Bool)
getGameStatus state color = 
    (\kingCell -> 
        (\(l1,l2) -> 
            (\checkList ->
                (
                    length(checkList)>0,
                    (length(checkList)>0) && (not (canKingMove state color kingCell)) && (not $ canAttackCheckPiece state color kingCell (l1,l2) checkList)
                )
            ) $ filter (\x -> x>= 0) (l1++l2)
        ) $ getCheckPositions state (colorCompliment color) False kingCell
    ) $ getKingPos state color

isInCheck :: GameState -> PColor -> Bool
isInCheck state color = checkForGameCheck state color False (getKingPos state color)

checkForGameCheck :: GameState -> PColor -> Bool -> Int -> Bool
checkForGameCheck state color firstIteration kingCell =
  let (l1,l2) = getCheckPositions state (colorCompliment color) firstIteration kingCell in
  let checkList = filter(\x -> x>= 0) (l1++l2) in
  (length(checkList) > 0)

-- This is for reference, dont remove this
-- checkMate :: GameState -> PColor -> Int -> Bool
-- checkMate state color kingCell
--     | length l == 0 = False
--     | (canKingMove state color kingCell) = False
--     | otherwise = not $ canAttackCheckPiece state color kingCell (l1,l2) l
--     where
--         (l1,l2) = getCheckPositions state (colorCompliment color) False kingCell
--         l = filter(\x -> x>= 0) (l1++l2)

canKingMove :: GameState -> PColor -> Int -> Bool
canKingMove state color cell =
  let r = cell `div` 8 in
  let c = cell `mod` 8 in
  let l = filter (\x -> (getSquareColorAt state x) /= color) $ map getCellIndex [(rr,cc) | rr <- [r-1,r,r+1], cc <- [c-1,c,c+1], rr>=0 , cc>=0, rr<=7, cc<=7, (rr,cc)/=(r,c)] in
  not (foldr (&&) True (map (checkForGameCheck state color False) l))

dummy :: GameState -> PColor -> Int -> IO ()
dummy state color cell = do
  let r = cell `div` 8
  let c = cell `mod` 8
  let l = [(rr,cc) | rr <- [r-1,r,r+1], cc <- [c-1,c,c+1], rr>=0 , cc>=0, rr<=7, cc<=7, (rr,cc)/=(r,c)]
  print l
  print $ map getCellIndex l

canAttackCheckPiece :: GameState -> PColor -> Int -> ([Int],[Int]) -> [Int] -> Bool
canAttackCheckPiece state color cell (list1,list2) checkList
  | (length(checkList) >= 2) = False
  | (length(checkList) == 1) =
    if (length(filter(\x -> x>= 0) (list2)) == 1) then canKillHorse state (colorCompliment color) (checkList !! 0)
    else canBlockCheckPiece state (colorCompliment color) cell (list1,list2) checkList
canAttackCheckPiece _ _ _ _ _ = True

canKillHorse ::  GameState -> PColor -> Int -> Bool
canKillHorse state color cell = checkForGameCheck state color True cell

canBlockCheckPiece ::  GameState -> PColor -> Int -> ([Int],[Int]) -> [Int] -> Bool
canBlockCheckPiece state color cell (list1,list2) checkList
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

checkRightColHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkRightColHit state color startRow endCol startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow], cc <- [(startCol+1),(startCol+2)..endCol]] in
    foldr (||) False (map (checkForGameCheck state color False) l)

checkLeftColHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkLeftColHit state color startRow endCol startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow], cc <- [startCol-1,startCol-2..endCol]] in
    foldr (||) False (map (checkForGameCheck state color False) l)

checkUpRowHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkUpRowHit state color startRow endRow startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow-1,startRow-2..endRow], cc <- [startCol]] in
    foldr (||) False (map (checkForGameCheck state color False) l)

checkDownRowHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkDownRowHit state color startRow endRow startCol =
  let l = map getCellIndex [(rr,cc) | rr <- [startRow+1,startRow+2..endRow], cc <- [startCol]] in
    foldr (||) False (map (checkForGameCheck state color False) l)

checkUpperLeftDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkUpperLeftDiagHit state color startRow endCell startCol =
    (\endRow endCol -> 
      let l = map getCellIndex [(rr,cc) | rr <- [startRow-1,startRow-2..endRow], cc <- [startCol-1,startCol-2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
        foldr (||) False (map (checkForGameCheck state color False) l)
    ) (endCell `div` 8) (endCell `mod` 8)

checkLowerLeftDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkLowerLeftDiagHit state color startRow endCell startCol =
    (\endRow endCol -> 
      let l = map getCellIndex [(rr,cc) | rr <- [startRow+1,startRow+2..endRow], cc <- [startCol-1,startCol-2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
        foldr (||) False (map (checkForGameCheck state color False) l)
    ) (endCell `div` 8) (endCell `mod` 8)

checkUpperRightDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkUpperRightDiagHit state color startRow endCell startCol =
    (\endRow endCol -> 
      let l = map getCellIndex [(rr,cc) | rr <- [startRow-1,startRow-2..endRow], cc <- [startCol+1,startCol+2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
        foldr (||) False (map (checkForGameCheck state color False) l)
    ) (endCell `div` 8) (endCell `mod` 8)

checkLowerRightDiagHit :: GameState -> PColor -> Int -> Int -> Int -> Bool
checkLowerRightDiagHit state color startRow endCell startCol =
    (\endRow endCol -> 
      let l = map getCellIndex [(rr,cc) | rr <- [startRow+1,startRow+2..endRow], cc <- [startCol+1,startCol+2..endCol], abs(rr-startRow) == abs(cc-startCol)] in
        foldr (||) False (map (checkForGameCheck state color False) l)
    ) (endCell `div` 8) (endCell `mod` 8)
{----------------- /Booleans if it can Hit ---------------------}

getCellIndex :: (Int,Int) -> Int
getCellIndex (x,y) = x*8 + y

{----------------- Position where it can Hit ---------------------}

checkPawnCheck7 :: GameState -> PColor -> Int -> Int
checkPawnCheck7 state color cell
    | checkTopDownHit
        && (((cell-7) >= 0) && (getSquareAt state (cell-7)) == (Piece color Pawn)) 
        = cell-7
    | (not checkTopDownHit)
        && (((cell+7) <= 63) && (getSquareAt state (cell+7)) == (Piece color Pawn)) 
        = cell+7
    | otherwise = -1
    where
        mycolor = if (getTurn state)==PlayerW then White else Black
        checkTopDownHit = (mycolor/=color)

checkPawnCheck9 :: GameState -> PColor -> Int -> Int
checkPawnCheck9 state color cell
    | checkTopDownHit
        && (((cell-9) >= 0) && (getSquareAt state (cell-9)) == (Piece color Pawn)) 
        = cell-9
    | (not checkTopDownHit)
        && (((cell+9) <= 63) && (getSquareAt state (cell+9)) == (Piece color Pawn)) 
        = cell+9
    | otherwise = -1
    where
        mycolor = if (getTurn state)==PlayerW then White else Black
        checkTopDownHit = (mycolor/=color)

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
{----------------- / Position where it can Hit ---------------------}
