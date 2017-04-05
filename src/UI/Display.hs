module UI.Display (display,drawSquare) where
 
import Graphics.UI.GLUT
import Control.Monad
import UI.Square
import Data.IORef


toggleColor :: GLfloat -> GLfloat
toggleColor c = if c==1 then 0
                else 1

drawSquare :: (GLfloat,GLfloat,GLfloat,GLfloat) -> IO ()
drawSquare (x,y,z,c) = preservingMatrix $ do
            color $ Color3 c c c
            translate $ Vector3 x y z
            drawCube

display :: IORef [(GLfloat,GLfloat,GLfloat,GLfloat)] -> DisplayCallback
display displayPoints = do
    dpoints <- get displayPoints
    clear [ColorBuffer]
    forM_ dpoints $ drawSquare
    flush

