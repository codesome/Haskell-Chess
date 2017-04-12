module UI.Bindings (display, reshape, keyboardMouse, opponentMoveHandler) where
 
import Graphics.UI.GLUT
import Control.Monad
import UI.Display
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Data.IORef
import Types
import BoardUtils
import GameUtils
import MoveUtils
import Network
import Control.Concurrent
import SocketHandlers
import Data.List.Split
import UI.ConsoleDisplay

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
    | (oddRow && (not oddCol)) || ((not oddRow) && oddCol)  = setBoardPointColorAt gameState index blackColor
    | otherwise = setBoardPointColorAt gameState index whiteColor
    where 
        row = index `div` 8
        col = index `mod` 8
        oddRow = (row `mod` 2)==0
        oddCol = (col `mod` 2)==0

resetToState :: GameState -> GameState -> GameState
resetToState previous _ = previous

leftButtonHandler :: GLint -> GLint -> GameState -> GameState
leftButtonHandler x y gameState
    | (not (isStartPointSet gameState)) = 
        (setStartPointIsSet 
            (setStartPoint 
                (colorThisAsFirst gameState index) 
                index
            ) 
            True
        )
    | (previousStart==index) = 
        (setStartPointIsSet 
            (resetColor gameState index) 
            False
        )
    | otherwise = 
        (setStartPoint 
            (colorThisAsFirst 
                (resetColor 
                    gameState 
                    (getStartPoint gameState)
                ) 
                index
            ) 
            index
        )
    where
        index = getIndex x y
        previousStart = getStartPoint gameState

rightButtonHandlerUtil :: GLint -> GLint -> GameState -> GameState
rightButtonHandlerUtil x y gameState
    | (isStartPointSet gameState) && (verifyMove gameState startPoint index) = 
        (setStartPointIsSet 
            (resetColor 
                (resetColor 
                    (moveFromTo 
                        (disableMove gameState) 
                        startPoint 
                        index
                    ) 
                    startPoint
                ) 
                index
            ) 
            False
        )
    | otherwise = gameState
    where
        index = getIndex x y
        startPoint = getStartPoint gameState

rightButtonHandler :: GLint -> GLint -> IORef GameState -> Socket -> IORef (String -> IO ()) -> IO ()
rightButtonHandler x y gameState sock s = (get gameState) >>= (\gstate ->
    if (getMoveEnabled gstate)
        then (\startPoint ->
            (gameState $~! (rightButtonHandlerUtil x y)) >>= 
                (\_ -> 
                (get gameState) >>= (\gstate2 -> 
                        if not (getMoveEnabled gstate2)
                            then (\pcolor ->
                                if (checkForGameCheck gstate2 (getKingPos gstate2 pcolor) pcolor)
                                    then 
                                        (gameState $~! (resetToState gstate)) >>=
                                            (\_ -> addMessage "Watch out! You will get into check.")
                                    else do
                                        (get s) >>= (\sender -> 
                                                sender ((show (63-startPoint))++":"++(show $ (63-(getIndex x y))))
                                            )
                                        forkIO $ opponentMoveHandler gameState sock
                                        updateConsole False False
                                ) $ getSquareColor (getSquareAt gstate startPoint)
                            else 
                                addMessage "That move is invalid"
                    )
                )
            ) $ getStartPoint gstate
        else 
            putStr ""
    )

opponentMove :: Int -> Int -> GameState -> GameState
opponentMove from to gameState =  enableMove $ moveFromTo gameState from to

opponentMoveHandlerUtil :: IORef GameState -> String -> IO ()
opponentMoveHandlerUtil gameState move =
    (\(from,to) ->
        (gameState $~! (opponentMove from to)) >>=
            (\_ ->
                (get gameState) >>= 
                    (\gstate ->
                        (\pcolor ->
                            if (checkForGameCheck gstate (getKingPos gstate pcolor) pcolor)
                                then updateConsole True True
                                else updateConsole True False
                        ) $ if (getTurn gstate)==PlayerW then White else Black
                    ) 
            )
    ) $ (\l -> (read (l!!0) :: Int, read (l!!1) :: Int)) $ (splitOn ":" move)

opponentMoveHandler :: IORef GameState -> Socket -> IO ()
opponentMoveHandler gameState sock = handleMessage sock $ opponentMoveHandlerUtil gameState

keyboardMouse :: IORef GameState -> Socket -> IORef (String -> IO ()) -> KeyboardMouseCallback
keyboardMouse gameState sock sender _key Down _ (Position x y) = case _key of
    (MouseButton LeftButton) -> gameState $~! (leftButtonHandler x y)
    (MouseButton RightButton) -> (rightButtonHandler x y gameState sock sender)
    (Char 'q') -> exitWith ExitSuccess
    _ -> return ()
keyboardMouse _ _ _ _ _ _ _ = return ()
