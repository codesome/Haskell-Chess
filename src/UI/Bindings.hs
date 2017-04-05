module UI.Bindings (display, reshape, keyboardMouse) where
 
import Graphics.UI.GLUT
import Control.Monad
import UI.Display
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Data.IORef

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

setPointAt :: [(GLfloat,GLfloat,GLfloat,GLfloat)] -> Int -> (GLfloat,GLfloat,GLfloat,GLfloat) -> [(GLfloat,GLfloat,GLfloat,GLfloat)]
setPointAt allPoints index newPoint = do
    let (l1, _:l2) = splitAt index allPoints
    l1 ++ [newPoint] ++ l2

toggleColor :: (GLfloat,GLfloat,GLfloat,GLfloat) -> (GLfloat,GLfloat,GLfloat,GLfloat)
toggleColor (x,y,z,c) = if c==1 then (x,y,z,0) else (x,y,z,1)

idn :: GLint -> GLint -> [(GLfloat,GLfloat,GLfloat,GLfloat)] -> [(GLfloat,GLfloat,GLfloat,GLfloat)]
idn x y allPoints = do
    let index = getIndex x y
    setPointAt allPoints index (toggleColor (allPoints!!index))

keyboardMouse :: IORef [(GLfloat,GLfloat,GLfloat,GLfloat)] -> KeyboardMouseCallback
keyboardMouse displayPoints _key Down _ (Position x y) = case _key of
  (MouseButton LeftButton) -> displayPoints $~! (idn x y)
  _ -> return ()
keyboardMouse _ _ _ _ _ = return ()
