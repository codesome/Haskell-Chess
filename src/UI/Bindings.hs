module UI.Bindings (
    display, 
    reshape, 
    keyboardMouse, 
    opponentMoveHandler
) where
 
import Graphics.UI.GLUT
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.List.Split
import Network
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import UI.Display
import UI.ConsoleDisplay
import Types
import BoardUtils
import GameUtils
import MoveUtils
import SocketHandlers

-- reshape callback
reshape :: ReshapeCallback
reshape size =
    -- viewport $= (Position 0 0, (Size 600 600))
    windowSize $= Size 600 600

-- get row/column from screen x y coordinates
getXYIndex :: GLint -> Int
getXYIndex x 
    | x<75      = 0
    | x<150     = 1
    | x<225     = 2
    | x<300     = 3
    | x<375     = 4
    | x<450     = 5
    | x<525     = 6
    | otherwise = 7

-- get index (0-63) from screen x y coordinates
getIndex :: GLint -> GLint -> Int
getIndex x y = (8*(getXYIndex y)) + (getXYIndex x)

-- color of first click quare
firstClickColor :: (GLfloat,GLfloat,GLfloat)
firstClickColor = (0,0.8,1)

-- white color
whiteColor :: (GLfloat,GLfloat,GLfloat)
whiteColor = (1,1,1)

-- black color
blackColor :: (GLfloat,GLfloat,GLfloat)
blackColor = (0,0,0)

-- color a square as first click square (firstClickColor)
colorThisAsFirst :: GameState -> Int -> GameState
colorThisAsFirst gameState index = setBoardPointColorAt gameState index firstClickColor

-- reset square color to original state
resetColor :: GameState -> Int -> GameState
resetColor gameState index
    | (oddRow && (not oddCol)) || ((not oddRow) && oddCol)  
                = setBoardPointColorAt gameState index blackColor
    | otherwise 
                = setBoardPointColorAt gameState index whiteColor
    where 
        row    = index `div` 8
        col    = index `mod` 8
        oddRow = (row `mod` 2)==0
        oddCol = (col `mod` 2)==0

-- reset a GameState to given state
resetToState :: GameState -> GameState -> GameState
resetToState previous _ = previous

-- Handler of left button click event
leftButtonHandler :: GLint -> GLint -> GameState -> GameState
leftButtonHandler x y gameState
    | (not (isStartPointSet gameState)) = -- start point is not set 
        (setStartPointIsSet -- making start point is set
            (setStartPoint  -- setting the start point
                (colorThisAsFirst gameState index) 
                index
            ) 
            True
        )
    -- below is if start point was set earlier
    | (previousStart==index) = -- start point is same as before
        (setStartPointIsSet              -- making start point is not set
            (resetColor gameState index) -- reset color
            False
        )
    | otherwise = -- different square selected
        (setStartPoint        -- setting start point
            (colorThisAsFirst -- coloring new start point square
                (resetColor   -- resetting old start point square
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
    -- start point is set and move is valid
    | (isStartPointSet gameState) && (verifyMove gameState startPoint index) = 
        (setStartPointIsSet                 -- making start point is not set 
            (resetColor                     -- resetting start point color
                (moveFromTo                 -- moving the piece
                    (disableMove gameState) -- disabling the move
                    startPoint 
                    index
                ) 
                startPoint
            ) 
            False
        )
    -- ignore otherwise
    | otherwise = gameState
    where
        index      = getIndex x y
        startPoint = getStartPoint gameState

rightButtonHandler :: GLint -> GLint -> IORef GameState -> Socket -> IORef (String -> IO ()) -> IO ()
rightButtonHandler x y gameState sock s = (get gameState) >>= (\gstate ->
    if (getMoveEnabled gstate) then -- if move is enabled
        (\startPoint -> -- after getting start point
            (gameState $~! (rightButtonHandlerUtil x y)) >>= (\_ -> -- validating and moving 
                (get gameState) >>= (\gstate2 ->  -- new updated game state
                    if not (getMoveEnabled gstate2) -- if move is not enabled (the move was valid and moved)
                        then (\pcolor ->
                            if (isInCheck gstate2 pcolor) -- checking for self check
                                then (gameState $~! (resetToState gstate)) >>= -- resetting to old state, invalid move
                                        (\_ -> addMessage "Watch out! You will get into check.")
                                else (get s) >>= -- not check, send move to opponent
                                        (\sender -> sender ((show (63-startPoint))++":"++(show $ (63-(getIndex x y)))) ) >>= 
                                        (\_ -> forkIO $ opponentMoveHandler gameState sock ) >>= 
                                        (\_ -> updateConsole False False False)
                            ) $ getSquareColor (getSquareAt gstate startPoint)
                        else 
                            addMessage "That move is invalid"
                )
            )
        ) $ getStartPoint gstate
        else 
            putStr ""
    )

pieceKillMessage :: IORef GameState -> Int -> IO ()
pieceKillMessage gameState to = 
    (get gameState) >>= (\gstate -> 
        (\piece -> 
            if piece==Empty
                then putStr ""
                else addMessage $ "Your " ++ (getPTypeStr $ getSquareType piece) ++ " was killed!"
        ) $ getSquareAt gstate to
    )

opponentMove :: Int -> Int -> GameState -> GameState
opponentMove from to gameState =  enableMove $ moveFromTo gameState from to

-- utility for opponent move handler
opponentMoveHandlerUtil :: IORef GameState -> String -> IO ()
opponentMoveHandlerUtil gameState move =
    (\(from,to) -> -- gets 'from' and 'to' from 'move'
        (pieceKillMessage gameState to) >>= (\_ -> -- display message if any piece killed
            (gameState $~! (opponentMove from to)) >>= (\_ -> -- register the move
                (get gameState) >>= (\gstate -> -- checking updated state for a check
                    (\pcolor -> 
                        (\(check,mate) -> 
                                updateConsole True check mate
                        ) $ getGameStatus gstate pcolor
                    ) $ if (getTurn gstate)==PlayerW then White else Black
                ) 
            )
        )
    ) $ (\l -> (read (l!!0) :: Int, read (l!!1) :: Int)) $ (splitOn ":" move)

-- handles opponent move
opponentMoveHandler :: IORef GameState -> Socket -> IO ()
opponentMoveHandler gameState sock = handleMessage sock $ opponentMoveHandlerUtil gameState

-- to handle mouse and keyboard interrupts
keyboardMouse :: IORef GameState -> Socket -> IORef (String -> IO ()) -> KeyboardMouseCallback
keyboardMouse gameState sock sender _key Down _ (Position x y) = 
    case _key of
        (MouseButton LeftButton)  -> gameState $~! (leftButtonHandler x y)
        (MouseButton RightButton) -> (rightButtonHandler x y gameState sock sender)
        (Char 'q')                -> exitWith ExitSuccess -- quit on 'q'
        otherwise                 -> return ()
keyboardMouse _ _ _ _ _ _ _ = return ()
