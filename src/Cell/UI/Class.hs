module Cell.UI.Class (Drawable(..)) where

import Cell.UI.Types
import Cell.Automata.Types
import Cell.UI.Attr

import Data.Array

import Brick

import Control.Monad
--import Control.Monad.Trans.Class

class Drawable a n where
    draw :: a -> Widget n

instance Drawable BinaryCell a where
    draw cell = padLeftRight 1 cellWidget
      where
        cellWidget = case cell of
          Empty -> withCellAttr DeadAttr $ str "."
          Full -> withCellAttr AliveAttr $ str "x"

instance (Drawable cell a) => Drawable (Grid cell) a where
    draw grid = vBox (fmap hBox rows)
      where
        rows = [ [ renderedCells ! (row, col) | col <- [ colBegin .. colEnd] ]
               | row <- [ rowBegin .. rowEnd ]
               ]

        renderedCells = fmap draw grid

        ((rowBegin, colBegin), (rowEnd, colEnd)) = bounds grid
