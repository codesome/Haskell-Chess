module UI.Bindings (display, reshape, keyboardMouse) where
 
import Graphics.UI.GLUT
import Control.Monad
import UI.Display
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Data.IORef
import Types
import BoardUtils
import MoveUtils

reshape :: ReshapeCallback
reshape size = do 
    -- viewport $= (Position 0 0, (Size 600 600))
    windowSize $= Size 600 600

getXYIndex :: GLint -> Int
getXYIndex x 
    | x<75 = 0
    | x<150 = 1
    | x<225 = 2
    | x<300 = 3
    | x<375 = 4
    | x<450 = 5
    | x<525 = 6
    | otherwise = 7

getIndex :: GLint -> GLint -> Int
getIndex x y = (8*(getXYIndex y)) + (getXYIndex x)

firstMoveColor :: (GLfloat,GLfloat,GLfloat)
firstMoveColor = (0,0.8,1)

whiteColor :: (GLfloat,GLfloat,GLfloat)
whiteColor = (1,1,1)

blackColor :: (GLfloat,GLfloat,GLfloat)
blackColor = (0,0,0)

colorThisAsFirst :: GameState -> Int -> GameState
colorThisAsFirst gameState index = setBoardPointColorAt gameState index firstMoveColor

resetColor :: GameState -> Int -> GameState
resetColor gameState index
    | (oddRow && oddCol) || ((not oddRow) && (not oddCol))  = setBoardPointColorAt gameState index blackColor
    | otherwise = setBoardPointColorAt gameState index whiteColor
    where 
        row = index `div` 8
        col = index `mod` 8
        oddRow = (row `mod` 2)==0
        oddCol = (col `mod` 2)==0


leftButtonHandler :: GLint -> GLint -> GameState -> GameState
leftButtonHandler x y gameState = do
    let index = getIndex x y
    if (isStartPointSet gameState)
        then do
            let previousStart = getStartPoint gameState
            if previousStart==index
                then do
                    (setStartPointIsSet (resetColor gameState index) False)
                else do
                    let intermediate = (resetColor gameState (getStartPoint gameState))
                    let intermediate2 = (colorThisAsFirst intermediate index)
                    (setStartPoint intermediate2 index)
        else do
            let intermediate = (colorThisAsFirst gameState index)
            (setStartPointIsSet (setStartPoint intermediate index) True)

rightButtonHandler :: GLint -> GLint -> GameState -> GameState
rightButtonHandler x y gameState = do
    let index = getIndex x y
    if (isStartPointSet gameState)
        then do
            let startPoint = getStartPoint gameState
            if (verifyMove gameState startPoint index)
                then do
                    let intermediate = togglePlayer (moveFromTo gameState startPoint index)
                    let intermediate1 = (resetColor intermediate startPoint)
                    (setStartPointIsSet (resetColor intermediate1 index) False)
                else 
                    gameState

        else
            gameState

keyboardMouse :: IORef GameState -> KeyboardMouseCallback
keyboardMouse gameState _key Down _ (Position x y) = case _key of
    (MouseButton LeftButton) -> gameState $~! (leftButtonHandler x y)
    (MouseButton RightButton) -> gameState $~! (rightButtonHandler x y)
    (Char 'q') -> exitWith ExitSuccess
    _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
