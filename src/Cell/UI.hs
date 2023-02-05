module Cell.UI
  ( displayGrid
  , withCellAttr
  , cellAttrMap
  , handleInput
  , defaultKeyMap
  , Tick(..)
  , Name(..)
  ) where

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Border.Style as BorderStyle
import           Brick.Widgets.Center

import           Cell.Automata
import           Cell.UI.Types
import           Cell.UI.Draw
import           Cell.UI.InputHandler

import           Data.Array
import           Data.Function              ( (&) )
import qualified Data.Map                   as M

import qualified Graphics.Vty               as Vty

displayGrid :: String -> (Int, Int) -> Grid BinaryCell -> Widget Name
displayGrid title selectionPos = hCenter . vCenter . withBorderStyle BorderStyle.unicode
  . borderWithLabel (str title) . drawGrid selectionPos
