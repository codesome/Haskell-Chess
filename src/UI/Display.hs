module UI.Display (display) where
 
import Graphics.UI.GLUT
import Control.Monad
import Data.IORef

import UI.Figures
import Types
import BoardUtils

drawSquare :: BoardSquare -> IO ()
drawSquare ((x,y,z),(r,g,b),p) = preservingMatrix $ do
            color $ Color3 r g b
            translate $ Vector3 x y z
            drawCube
            drawPiece p

display :: IORef GameState -> DisplayCallback
display gameState = (clear [ColorBuffer]) >>= (\_ -> 
            (get gameState) >>= (\gstate ->
                    (forM_ (getBoardPoints gstate) $ drawSquare) >>= (\_ -> flush )
                )
        )
