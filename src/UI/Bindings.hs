module UI.Bindings (display, reshape, keyboardMouse, opponentMoveHandler) where
 
import Graphics.UI.GLUT
import Control.Monad
import UI.Display
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Data.IORef
import Types
import BoardUtils
import MoveUtils
import Network
import Control.Concurrent
import SocketHandlers
import Data.List.Split

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

rightButtonHandlerUtil :: GLint -> GLint -> GameState -> GameState
rightButtonHandlerUtil x y gameState = do
    let index = getIndex x y
    if (isStartPointSet gameState)
        then do
            let startPoint = getStartPoint gameState
            if (verifyMove gameState startPoint index)
                then do
                    let intermediate = (disableMove gameState)
                    let intermediate1 = (moveFromTo intermediate startPoint index)
                    let intermediate2 = (resetColor intermediate1 startPoint)
                    (setStartPointIsSet (resetColor intermediate2 index) False)
                else 
                    gameState

        else do
            gameState

rightButtonHandler :: GLint -> GLint -> IORef GameState -> Socket -> IORef (String -> IO ()) -> IO ()
rightButtonHandler x y gameState sock s = do
    gstate <- get gameState
    if (getMoveEnabled gstate) 
        then do
            let startPoint = getStartPoint gstate
            gameState $~! (rightButtonHandlerUtil x y)

            gstate2 <- get gameState
            if not (getMoveEnabled gstate2)
                then do
                    sender <- get s
                    sender ((show (63-startPoint))++":"++(show $ (63-(getIndex x y))))
                    forkIO $ opponentMoveHandler gameState sock
                    putStr ""
                else 
                    putStr ""

        else 
            putStr ""

opponentMove :: Int -> Int -> GameState -> GameState
opponentMove from to gameState =  enableMove $ moveFromTo gameState from to

opponentMoveHandlerUtil :: IORef GameState -> String -> IO ()
opponentMoveHandlerUtil gameState move = do
    putStrLn move
    let l = splitOn ":" move
    let from = read (l!!0) :: Int
    let to = read (l!!1) :: Int
    gameState $~! (opponentMove from to)

opponentMoveHandler :: IORef GameState -> Socket -> IO ()
opponentMoveHandler gameState sock = handleMessage sock $ opponentMoveHandlerUtil gameState

keyboardMouse :: IORef GameState -> Socket -> IORef (String -> IO ()) -> KeyboardMouseCallback
keyboardMouse gameState sock sender _key Down _ (Position x y) = case _key of
    (MouseButton LeftButton) -> gameState $~! (leftButtonHandler x y)
    (MouseButton RightButton) -> (rightButtonHandler x y gameState sock sender)
    (Char 'q') -> exitWith ExitSuccess
    _ -> return ()
keyboardMouse _ _ _ _ _ _ _ = return ()
