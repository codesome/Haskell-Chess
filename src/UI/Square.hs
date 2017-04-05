module UI.Square where
 
import Graphics.UI.GLUT
 
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
 
drawCube :: IO ()
drawCube = renderPrimitive Quads $ mapM_ vertex3f
    [(0,0,0),(0,0.25,0),(0.25,0.25,0),(0.25,0,0)]
