module Cell.UI
  ( displayGrid
  , withCellAttr
  , cellAttrMap
  , Drawable(..)
  , Tick(..)
  , Name(..)
  ) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style as BorderStyle
import           Brick.Widgets.Center

import           Cell.Automata
import           Cell.UI.Attr
import           Cell.UI.Class
import           Cell.UI.Types

import           Data.Array
import           Data.Function              ( (&) )
import qualified Data.Map                   as M

import qualified Graphics.Vty               as Vty

displayGrid :: (Drawable cell Name) => String -> Grid cell -> Widget Name
displayGrid title = hCenter . vCenter . withBorderStyle BorderStyle.unicode
  . borderWithLabel (str title) . draw
