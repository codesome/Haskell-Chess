-- Used to debug in interpeter

module Test where

-- UI/
import UI.Bindings
import UI.ConsoleDisplay
import UI.Display
import UI.Figures

-- VerifyMove/
import qualified VerifyMove.Bishop as Bishop
import qualified VerifyMove.King as King
import qualified VerifyMove.Knight as Knight
import qualified VerifyMove.Pawn as Pawn
import qualified VerifyMove.Queen as Queen
import qualified VerifyMove.Rook as Rook

import BoardUtils
import Defaults
import GameUtils
import Main
import MoveUtils
import SocketHandlers
import Types
