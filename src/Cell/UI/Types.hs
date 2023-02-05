module Cell.UI.Types
  ( Tick(..)
  , Name(..)
  , CellAttr(..)
  , KeyInput(..)
  , CellSelectionStatus(..)
  ) where

import           Brick.Types

import           Cell.Automata.Types

data Tick = Tick

data CellAttr = CellSelectedAttr | CellNotSelectedAttr | CellAliveAttr | CellDeadAttr | EmptyAttr | CombinedAttr CellAttr CellAttr

data CellSelectionStatus = CellSelected | CellNotSelected

type Name = ()

data KeyInput =
    KeyUp
  | KeyDown
  | KeyLeft
  | KeyRight
  | KeyToggleCell
  | KeyToggleSimulation
  | KeyExit
