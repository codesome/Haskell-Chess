module Types where

import Graphics.UI.GLUT (GLfloat)


-- Board used in game
type Board       = [[Square]]

{- Piece attributes in a square -}
-- Single piece
data Square      = Piece PColor PType | Empty deriving (Show,Eq)

-- Color of the piece
data PColor      = White | Black | NoColor deriving (Show,Eq)

-- Type of the piece
data PType       = Bishop | King | Knight | Pawn | Queen | Rook | NoType deriving (Show,Eq)

-- Players
data Player      = PlayerW | PlayerB deriving (Show,Eq)

-- Display point on the board
type BoardSquare = ((GLfloat,GLfloat,GLfloat),(GLfloat,GLfloat,GLfloat),(Int,GLfloat))

emptyBoardPiece :: (Int,GLfloat)
emptyBoardPiece  = (0,0)

-- Short hand notations for pieces
wBishop, wKing, wKnight, wPawn, wQueen, wRook, bBishop, bKing, bKnight, bPawn, bQueen, bRook :: Square
wBishop = (Piece White Bishop)
wKing   = (Piece White King)
wKnight = (Piece White Knight)
wPawn   = (Piece White Pawn)
wQueen  = (Piece White Queen)
wRook   = (Piece White Rook)
bBishop = (Piece Black Bishop)
bKing   = (Piece Black King)
bKnight = (Piece Black Knight)
bPawn   = (Piece Black Pawn)
bQueen  = (Piece Black Queen)
bRook   = (Piece Black Rook)

-- State data used during the game
data GameState = GameState {
    board           :: Board, -- 2D list of Square
    boardPoints     :: [BoardSquare], -- display points
    turn            :: Player, -- PlayerW/PlayerB
    wasCheck        :: Bool, -- True if it was a check before this move
    whoWasInCheck   :: Maybe Player, -- Either PlayerW/PlayerB on check or Nothing
    inProgress      :: Bool, -- True if the game is still on
    whiteKing       :: Int, -- white king position
    blackKing       :: Int, -- black king position
    startPointIsSet :: Bool, -- True if left click is registered
    startPoint      :: Int, -- index of start square selected
    endPoint        :: Int, -- index of end square selected
    moveEnabled     :: Bool -- True if the user can move
} deriving (Show)
