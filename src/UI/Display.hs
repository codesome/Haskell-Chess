module UI.Display (display,drawSquare) where
 
import Graphics.UI.GLUT
import Control.Monad
import UI.Square
import Data.IORef
import Types
import BoardUtils

toggleColor :: GLfloat -> GLfloat
toggleColor c = if c==1 then 0
                else 1

drawSquare :: BoardSquare -> IO ()
drawSquare ((x,y,z),(r,g,b)) = preservingMatrix $ do
            color $ Color3 r g b
            translate $ Vector3 x y z
            drawCube

display :: IORef GameState -> DisplayCallback
display gameState = do
    gstate <- get gameState
    clear [ColorBuffer]
    forM_ (getBoardPoints gstate) $ drawSquare
    flush
