module UI.Figures (drawSquare, drawPiece) where
 
import Graphics.UI.GLUT
import Control.Monad

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

-- draws a square
drawSquare :: IO ()
drawSquare = renderPrimitive Quads $ mapM_ vertex3f
    [(0,0,0),(0,0.25,0),(0.25,0.25,0),(0.25,0,0)]

-- draws a polygon
drawPolygon :: [(Float,Float,Float)] -> IO ()
drawPolygon vertices = renderPrimitive Polygon $ mapM_ vertex3f vertices

-- draws outline
drawLoop :: [(Float,Float,Float)] -> IO ()
drawLoop vertices = renderPrimitive LineLoop $ mapM_ vertex3f vertices

-- draws a piece
drawPiece :: (Int, GLfloat) -> IO ()
drawPiece (p, c) = do
    color $ Color3 c c c
    drawPieceFigure p c

invertColor :: GLfloat -> GLfloat
invertColor c
    | c==0.05   = 0.95
    | otherwise = 0.05

drawPieceFigure :: Int -> GLfloat -> IO ()
drawPieceFigure p c
    -- bishop
    | p==1      = drawBishop c
    -- king
    | p==2      = do drawKing; color $ invertedColor ; drawKingLoop;
    -- knight
    | p==3      = drawKnight c
    -- pawn
    | p==4      = drawPawn c
    -- queen
    | p==5      = do drawQueen; color $ invertedColor ; drawQueenLoop;
    -- rook
    | p==6      = do drawRook; color $ invertedColor ; drawRookLoop;
    -- empty
    | otherwise = putStr ""
    where
        inverted = invertColor c
        invertedColor = Color3 inverted inverted inverted

--------- ROOK
rookVertices :: [[(Float, Float, Float)]]
rookVertices = [
        -- lower Quad
        [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ],
        -- middle Quad
        [ (0.0875, 0.0625, 0), (0.0875, 0.1875, 0), (0.1625, 0.1875, 0), (0.1625, 0.0625, 0) ],
        -- upper Quad
        [ (0.0625, 0.1875, 0), (0.0625, 0.225, 0), (0.1875, 0.225, 0), (0.1875, 0.1875, 0) ]
    ]
    
drawRook :: IO ()
drawRook = forM_ rookVertices $ drawPolygon

drawRookLoop :: IO ()
drawRookLoop = forM_ rookVertices $ drawLoop


---------- PAWN
pawnVertices1 :: [[(Float,Float,Float)]]
pawnVertices1 = [ 
        -- middle quad
        [ (0.0875, 0.0625, 0), (0.125, 0.1875, 0), (0.1625, 0.0625, 0) ] 
    ]

pawnVertices2 :: [[(Float,Float,Float)]]
pawnVertices2 = [ 
        -- lower quad
        [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ],
        -- circle
        [ (0.125+ 0.04*(cos (2*pi*k/20)), 0.15+ 0.04*(sin (2*pi*k/20)), 0) | k <- [1..20] ]
    ]

drawPawn :: GLfloat -> IO ()
drawPawn c = (\inv -> do
        forM_ pawnVertices1 $ drawPolygon

        color $ Color3 inv inv inv 
        drawPawnLoop1
        
        color $ Color3 c c c
        forM_ pawnVertices2 $ drawPolygon

        color $ Color3 inv inv inv 
        drawPawnLoop2
    ) $ invertColor c


drawPawnLoop1 :: IO ()
drawPawnLoop1 = forM_ pawnVertices1 $ drawLoop

drawPawnLoop2 :: IO ()
drawPawnLoop2 = forM_ pawnVertices2 $ drawLoop


-------- BISHOP
bishopVertices1 :: [[(Float,Float,Float)]]
bishopVertices1 = [
        -- lower Quad
        [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ],
        -- middle Quad
        [ (0.0875, 0.0625, 0), (0.125, 0.1875, 0), (0.1625, 0.0625, 0) ],
        -- small cap
        [ (0.1, 0.18, 0), (0.125, 0.23, 0), (0.15, 0.18, 0) ]
    ]

bishopVertices2 :: [[(Float,Float,Float)]]
bishopVertices2 = [
        -- big circle
        [ (0.125+ 0.045*(cos (2*pi*k/20)), 0.15+ 0.045*(sin (2*pi*k/20)), 0) | k <- [1..20] ]
    ]

drawBishop :: GLfloat -> IO ()
drawBishop c = (\inv -> do
        forM_ bishopVertices1 $ drawPolygon
        
        color $ Color3 inv inv inv 
        drawBishopLoop1
        
        color $ Color3 c c c
        forM_ bishopVertices2 $ drawPolygon

        color $ Color3 inv inv inv 
        drawBishopLoop2
    ) $ invertColor c


drawBishopLoop1 :: IO ()
drawBishopLoop1 = forM_ bishopVertices1 $ drawLoop

drawBishopLoop2 :: IO ()
drawBishopLoop2 = forM_ bishopVertices2 $ drawLoop

-------- KING
kingVertices :: [[(Float,Float,Float)]]
kingVertices = [
        -- lower Quad
        [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ],
        -- middle Quad
        [ (0.0875, 0.0625, 0), (0.0875, 0.125, 0), (0.1625, 0.125, 0), (0.1625, 0.0625, 0) ],
        -- upper Quad
        [ (0.075, 0.125, 0), (0.075, 0.15, 0), (0.175, 0.15, 0), (0.175, 0.125, 0) ],
        
        -- cross
        [ (0.0828125, 0.17625, 0), (0.0828125, 0.204375, 0), (0.1671875, 0.204375, 0), (0.1671875, 0.17625, 0) ],
        [ (0.1109375, 0.15, 0), (0.1109375, 0.2325, 0), (0.1390625, 0.2325, 0), (0.1390625, 0.15, 0) ]
    ]

drawKing :: IO ()
drawKing = forM_ kingVertices $ drawPolygon

drawKingLoop :: IO ()
drawKingLoop = forM_ kingVertices $ drawLoop

--------- QUEEN
queenVertices :: [[(Float,Float,Float)]]
queenVertices = [
        -- lower Quad
        [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ],
        -- middle Quad
        [ (0.0875, 0.0625, 0), (0.1, 0.15, 0), (0.15, 0.15, 0), (0.1625, 0.0625, 0) ],
        -- neck
        [ (0.0875, 0.15, 0), (0.1, 0.175, 0), (0.15, 0.175, 0), (0.1625, 0.15, 0) ],
        -- top
        [ (0.1, 0.175, 0), (0.075, 0.22, 0), (0.175, 0.22, 0), (0.15, 0.175, 0) ]
    ]

drawQueen :: IO ()
drawQueen = forM_ queenVertices $ drawPolygon

drawQueenLoop :: IO ()
drawQueenLoop = forM_ queenVertices $ drawLoop

--------- KNIGHT
knightVertices1 :: [[(Float,Float,Float)]]
knightVertices1 = [
        -- lower quad
        [ (0.05, 0.025, 0), (0.05, 0.0625, 0), (0.2, 0.0625, 0), (0.2, 0.025, 0) ],
        -- middle quad
        [ (0.0775, 0.0625, 0), (0.15, 0.2, 0), (0.165, 0.0625, 0) ]
    ]

knightVertices2 :: [[(Float,Float,Float)]]
knightVertices2 = [
        -- head
        [ (0.15, 0.22, 0), (0.157, 0.13, 0), (0.05, 0.1, 0), (0.05, 0.14, 0)],
        -- eye
        [ (0.115+ 0.01*(cos (2*pi*k/20)), 0.16+ 0.01*(sin (2*pi*k/20)), 0) | k <- [1..20] ]
    ]

drawKnight :: GLfloat -> IO ()
drawKnight c = (\inv -> do
        forM_ knightVertices1 $ drawPolygon

        color $ Color3 inv inv inv 
        drawKnightLoop1
        
        color $ Color3 c c c
        forM_ knightVertices2 $ drawPolygon

        color $ Color3 inv inv inv 
        drawKnightLoop2
    ) $ invertColor c

drawKnightLoop1 :: IO ()
drawKnightLoop1 = forM_ knightVertices1 $ drawLoop

drawKnightLoop2 :: IO ()
drawKnightLoop2 = forM_ knightVertices2 $ drawLoop
