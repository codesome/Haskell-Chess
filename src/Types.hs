module Types where

-- Board used in game
type Board = [[Square]]

{- Piece attributes in a square -}

-- Single piece
data Square = Piece PColor PType | Empty deriving (Show,Eq)

-- Color of the piece
data PColor = White | Black | NoColor deriving (Show,Eq)

-- Type of the piece
data PType = Bishop | King | Knight | Pawn | Queen | Rook deriving (Show,Eq)

-- Players
data Player = PlayerW | PlayerB deriving (Show,Eq)

-- Short hand notations for convienience
wBishop :: Square
wBishop = (Piece White Bishop)

wKing :: Square
wKing = (Piece White King)

wKnight :: Square
wKnight = (Piece White Knight)

wPawn :: Square
wPawn = (Piece White Pawn)

wQueen :: Square
wQueen = (Piece White Queen)

wRook :: Square
wRook = (Piece White Rook)

bBishop :: Square
bBishop = (Piece Black Bishop)

bKing :: Square
bKing = (Piece Black King)

bKnight :: Square
bKnight = (Piece Black Knight)

bPawn :: Square
bPawn = (Piece Black Pawn)

bQueen :: Square
bQueen = (Piece Black Queen)

bRook :: Square
bRook = (Piece Black Rook)

-- State data used during the game
data GameState = GameState {
    board :: Board, -- 2D list of Square
    turn :: Player, -- PlayerW/PlayerB depending on whose turn it is next
    wasCheck :: Bool, -- True if it was a check before this move
    whoWasInCheck :: Maybe Player, -- Either PlayerW/PlayerB on check or Nothing
    inProgress :: Bool, -- True if the game is still on
    whiteKing :: Int, -- white king position
    blackKing :: Int -- black king position
} deriving (Show)
