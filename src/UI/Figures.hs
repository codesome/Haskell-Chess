module UI.Figures where
 
import Graphics.UI.GLUT
 
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
 
drawCube :: IO ()
drawCube = renderPrimitive Quads $ mapM_ vertex3f
    [(0,0,0),(0,0.25,0),(0.25,0.25,0),(0.25,0,0)]

drawPolygon :: [(Float,Float,Float)] -> IO ()
drawPolygon l = renderPrimitive Polygon $ mapM_ vertex3f l

drawLoop :: [(Float,Float,Float)] -> IO ()
drawLoop l = renderPrimitive LineLoop $ mapM_ vertex3f l

drawPiece :: (Int, GLfloat) -> IO ()
drawPiece (p, c) = do
    color $ Color3 c c c
    drawPieceFigure p c

invertColor :: GLfloat -> GLfloat
invertColor c
    | c==0.05 = 0.95
    | otherwise = 0.05

drawPieceFigure :: Int -> GLfloat -> IO ()
drawPieceFigure p c
    | p==1 = do drawBishop; color $ invertedColor ; drawBishopLoop;
    | p==2 = do drawKing; color $ invertedColor ; drawKingLoop;
    | p==3 = do drawKnight; color $ invertedColor ; drawKnightLoop;
    | p==4 = do drawPawn; color $ invertedColor ; drawPawnLoop;
    | p==5 = do drawQueen; color $ invertedColor ; drawQueenLoop;
    | p==6 = do drawRook; color $ invertedColor ; drawRookLoop;
    | otherwise = putStr ""
    where
        inverted = invertColor c
        invertedColor = Color3 inverted inverted inverted

drawRook :: IO ()
drawRook = do
    -- lower Quad
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- middle Quad
    drawPolygon [ (0.0875, 0.0625, 0), (0.0875, 0.1875, 0), (0.1625, 0.1875, 0), (0.1625, 0.0625, 0) ]
    -- upper Quad
    drawPolygon [ (0.0625, 0.1875, 0), (0.0625, 0.225, 0), (0.1875, 0.225, 0), (0.1875, 0.1875, 0) ]

drawBishop :: IO () -- TODO
drawBishop = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]

drawKnight :: IO () -- TODO
drawKnight = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]

drawQueen :: IO () -- TODO
drawQueen = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]

drawKing :: IO () -- TODO
drawKing = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]

drawPawn :: IO () -- TODO
drawPawn = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]

drawRookLoop :: IO ()
drawRookLoop = do    
    -- lower Quad
    drawLoop [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- middle Quad
    drawLoop [ (0.0875, 0.0625, 0), (0.0875, 0.1875, 0), (0.1625, 0.1875, 0), (0.1625, 0.0625, 0) ]
    -- upper Quad
    drawLoop [ (0.0625, 0.1875, 0), (0.0625, 0.225, 0), (0.1875, 0.225, 0), (0.1875, 0.1875, 0) ]

drawBishopLoop :: IO () -- TODO
drawBishopLoop = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]

drawKnightLoop :: IO () -- TODO
drawKnightLoop = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]

drawQueenLoop :: IO () -- TODO
drawQueenLoop = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]

drawKingLoop :: IO () -- TODO
drawKingLoop = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]

drawPawnLoop :: IO () -- TODO
drawPawnLoop = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
