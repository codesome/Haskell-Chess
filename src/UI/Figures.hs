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
    | p==1 = drawBishop c
    | p==2 = do drawKing; color $ invertedColor ; drawKingLoop;
    | p==3 = do drawKnight; color $ invertedColor ; drawKnightLoop;
    | p==4 = drawPawn c
    | p==5 = do drawQueen; color $ invertedColor ; drawQueenLoop;
    | p==6 = do drawRook; color $ invertedColor ; drawRookLoop;
    | otherwise = putStr ""
    where
        inverted = invertColor c
        invertedColor = Color3 inverted inverted inverted

--------- ROOK
drawRook :: IO ()
drawRook = do
    -- lower Quad
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- middle Quad
    drawPolygon [ (0.0875, 0.0625, 0), (0.0875, 0.1875, 0), (0.1625, 0.1875, 0), (0.1625, 0.0625, 0) ]
    -- upper Quad
    drawPolygon [ (0.0625, 0.1875, 0), (0.0625, 0.225, 0), (0.1875, 0.225, 0), (0.1875, 0.1875, 0) ]

drawRookLoop :: IO ()
drawRookLoop = do    
    -- lower Quad
    drawLoop [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- middle Quad
    drawLoop [ (0.0875, 0.0625, 0), (0.0875, 0.1875, 0), (0.1625, 0.1875, 0), (0.1625, 0.0625, 0) ]
    -- upper Quad
    drawLoop [ (0.0625, 0.1875, 0), (0.0625, 0.225, 0), (0.1875, 0.225, 0), (0.1875, 0.1875, 0) ]

---------- PAWN
drawPawn :: GLfloat -> IO ()
drawPawn c = do
    -- middle quad
    drawPolygon [ (0.0875, 0.0625, 0), (0.125, 0.1875, 0), (0.1625, 0.0625, 0) ]
    -- drawPolygon [ (0.075, 0.0625, 0), (0.1, 0.125, 0), (0.15, 0.125, 0), (0.175, 0.0625, 0) ]

    let inv = invertColor c
    color $ Color3 inv inv inv 
    drawPawnLoop1
    
    color $ Color3 c c c
    -- lower quad
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- circle
    drawPolygon [ (0.125+ 0.04*(cos (2*pi*k/20)), 0.15+ 0.04*(sin (2*pi*k/20)), 0) | k <- [1..20] ]
    

    color $ Color3 inv inv inv 
    drawPawnLoop2

drawPawnLoop1 :: IO ()
drawPawnLoop1 = do
    -- middle quad
    drawLoop [ (0.0875, 0.0625, 0), (0.125, 0.1875, 0), (0.1625, 0.0625, 0) ]
    -- drawLoop [ (0.075, 0.0625, 0), (0.1, 0.125, 0), (0.15, 0.125, 0), (0.175, 0.0625, 0) ]

drawPawnLoop2 :: IO ()
drawPawnLoop2 = do
    -- lower quad
    drawLoop [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- circle
    drawLoop [ (0.125+ 0.04*(cos (2*pi*k/20)), 0.15+ 0.04*(sin (2*pi*k/20)), 0) | k <- [1..20] ]

-------- BISHOP
drawBishop :: GLfloat -> IO ()
drawBishop c = do

    -- lower Quad
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- middle Quad
    drawPolygon [ (0.0875, 0.0625, 0), (0.125, 0.1875, 0), (0.1625, 0.0625, 0) ]
    -- small cap
    drawPolygon [ (0.1, 0.18, 0), (0.125, 0.23, 0), (0.15, 0.18, 0) ]
    
    let inv = invertColor c
    color $ Color3 inv inv inv 
    drawBishopLoop1
    
    color $ Color3 c c c

    -- big circle
    drawPolygon [ (0.125+ 0.045*(cos (2*pi*k/20)), 0.15+ 0.045*(sin (2*pi*k/20)), 0) | k <- [1..20] ]

    color $ Color3 inv inv inv 
    drawBishopLoop2

drawBishopLoop1 :: IO ()
drawBishopLoop1 = do
    -- lower Quad
    drawLoop [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- middle Quad
    drawLoop [ (0.0875, 0.0625, 0), (0.125, 0.1875, 0), (0.1625, 0.0625, 0) ]
    -- small cap
    drawLoop [ (0.1, 0.18, 0), (0.125, 0.23, 0), (0.15, 0.18, 0) ]

drawBishopLoop2 :: IO ()
drawBishopLoop2 = do
    -- big circle
    drawLoop [ (0.125+ 0.045*(cos (2*pi*k/20)), 0.15+ 0.045*(sin (2*pi*k/20)), 0) | k <- [1..20] ]

-------- KING
drawKing :: IO ()
drawKing = do
    -- lower Quad
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- middle Quad
    drawPolygon [ (0.0875, 0.0625, 0), (0.0875, 0.125, 0), (0.1625, 0.125, 0), (0.1625, 0.0625, 0) ]
    -- upper Quad
    drawPolygon [ (0.075, 0.125, 0), (0.075, 0.15, 0), (0.175, 0.15, 0), (0.175, 0.125, 0) ]

    -- cross
    drawPolygon [  -- horizontal
            (0.0828125, 0.17625, 0), (0.0828125, 0.204375, 0), 
            (0.1671875, 0.204375, 0), (0.1671875, 0.17625, 0)
        ]
    drawPolygon [ -- vertical
            (0.1109375, 0.1, 0), (0.1109375, 0.2325, 0), 
            (0.1390625, 0.2325, 0), (0.1390625, 0.1, 0) 
        ]

drawKingLoop :: IO ()
drawKingLoop = do
    -- lower Quad
    drawLoop [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- middle Quad
    drawLoop [ (0.0875, 0.0625, 0), (0.0875, 0.125, 0), (0.1625, 0.125, 0), (0.1625, 0.0625, 0) ]
    -- upper Quad
    drawLoop [ (0.075, 0.125, 0), (0.075, 0.15, 0), (0.175, 0.15, 0), (0.175, 0.125, 0) ]

    -- cross
    drawLoop [ -- horizontal
            (0.0828125, 0.17625, 0), (0.0828125, 0.204375, 0), 
            (0.1671875, 0.204375, 0), (0.1671875, 0.17625, 0)
        ]
    drawLoop [ -- vertical
            (0.1109375, 0.15, 0), (0.1109375, 0.2325, 0), 
            (0.1390625, 0.2325, 0), (0.1390625, 0.15, 0) 
        ]

drawQueen :: IO ()
drawQueen = do
    -- lower Quad
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- middle Quad
    drawPolygon [ (0.0875, 0.0625, 0), (0.1, 0.1875, 0), (0.15, 0.1875, 0), (0.1625, 0.0625, 0) ]
    -- neck
    drawPolygon [ (0.0875, 0.15, 0), (0.1, 0.175, 0), (0.15, 0.175, 0), (0.1625, 0.15, 0) ]
    -- top
    drawPolygon [ (0.1, 0.175, 0), (0.075, 0.22, 0), (0.175, 0.22, 0), (0.15, 0.175, 0) ]

drawQueenLoop :: IO ()
drawQueenLoop = do
    -- lower Quad
    drawLoop [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]
    -- middle Quad
    drawLoop [ (0.0875, 0.0625, 0), (0.1, 0.15, 0), (0.15, 0.15, 0), (0.1625, 0.0625, 0) ]
    -- neck
    drawLoop [ (0.0875, 0.15, 0), (0.1, 0.175, 0), (0.15, 0.175, 0), (0.1625, 0.15, 0) ]
    -- top
    drawLoop [ (0.1, 0.175, 0), (0.075, 0.22, 0), (0.175, 0.22, 0), (0.15, 0.175, 0) ]


drawKnight :: IO () -- TODO
drawKnight = do
    drawPolygon [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]


drawKnightLoop :: IO () -- TODO
drawKnightLoop = do
    drawLoop [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ]



