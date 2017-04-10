module UI.Display (display,drawSquare) where
 
import Graphics.UI.GLUT
import Control.Monad
import UI.Figures
import Data.IORef
import Types
import BoardUtils

drawSquare :: BoardSquare -> IO ()
drawSquare ((x,y,z),(r,g,b),p) = preservingMatrix $ do
            color $ Color3 r g b
            translate $ Vector3 x y z
            drawCube
            drawPiece p

display :: IORef GameState -> DisplayCallback
display gameState = do
    clear [ColorBuffer]
    gstate <- get gameState
    forM_ (getBoardPoints gstate) $ drawSquare
    flush
