module Cell.UI.Attr ( withCellAttr, cellAttrMap ) where

import           Brick

import           Cell.UI.Types

import           Graphics.Vty  as Vty

cellAttrMap :: s -> AttrMap
cellAttrMap = const $
  attrMap Vty.defAttr
          [ (getCellAttr CellAttr, bg Vty.black)
          , (getCellAttr AliveAttr, fg Vty.green)
          , (getCellAttr DeadAttr, fg Vty.red)
          ]

getCellAttrName :: CellAttr -> AttrName
getCellAttrName CellAttr = attrName "cell.base"
getCellAttrName AliveAttr = attrName "cell.alive"
getCellAttrName DeadAttr = attrName "cell.dead"

getCellAttr :: CellAttr -> AttrName
getCellAttr CellAttr = getCellAttrName CellAttr
getCellAttr AliveAttr = getCellAttrName CellAttr <> getCellAttrName AliveAttr
getCellAttr DeadAttr = getCellAttrName CellAttr <> getCellAttrName DeadAttr

withCellAttr :: CellAttr -> Widget n -> Widget n
withCellAttr = withAttr . getCellAttr
