module Types where

-- Board used in game
type Board = [[Square]]

-- Single Square in the board
type Square = Maybe Piece

{- Piece attributes in a square -}

-- Single piece
data Piece = Piece PColor PType deriving (Show)

-- Color of the piece
data PColor = White | Black deriving (Show)

-- Type of the piece
data PType = Bishop | King | Knight | Pawn | Queen | Rook deriving (Show)

-- Players
data Player = PlayerW | PlayerB deriving (Show)

wBishop :: Maybe Piece
wBishop = Just (Piece White Bishop)

wKing :: Maybe Piece
wKing = Just (Piece White King)

wKnight :: Maybe Piece
wKnight = Just (Piece White Knight)

wPawn :: Maybe Piece
wPawn = Just (Piece White Pawn)

wQueen :: Maybe Piece
wQueen = Just (Piece White Queen)

wRook :: Maybe Piece
wRook = Just (Piece White Rook)

bBishop :: Maybe Piece
bBishop = Just (Piece Black Bishop)

bKing :: Maybe Piece
bKing = Just (Piece Black King)

bKnight :: Maybe Piece
bKnight = Just (Piece Black Knight)

bPawn :: Maybe Piece
bPawn = Just (Piece Black Pawn)

bQueen :: Maybe Piece
bQueen = Just (Piece Black Queen)

bRook :: Maybe Piece
bRook = Just (Piece Black Rook)

empty :: Maybe Piece
empty = Nothing

-- State data used during the game
data GameState = GameState {
    board :: Board, -- 2D list of Square
    turn :: Player, -- PlayerW/PlayerB depending on whose turn it is next
    wasCheck :: Bool, -- True if it was a check before this move
    whoWasInCheck :: Maybe Player, -- PlayerW/PlayerB/Nothing
    inProgress :: Bool -- True if the game is still on
} deriving (Show)
