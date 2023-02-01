module Cell.UI.Types ( Tick(..), Name(..), CellAttr(..) ) where

import           Brick.Types

import           Cell.Automata.Types

data Tick = Tick

data CellAttr = CellAttr | AliveAttr | DeadAttr

type Name = ()

