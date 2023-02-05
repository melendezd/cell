module Cell.UI.Draw ( withCellAttr, cellAttrMap, drawGrid ) where

import           Brick

import           Cell.Automata.Types
import           Cell.UI.Types
import           Cell.Util

import           Data.Array
import           Data.Bifunctor      ( first, second )
import           Data.Monoid

import           Graphics.Vty        as Vty

--------------------------------------------------------------------------------
-- Attr
--------------------------------------------------------------------------------
cellAttrMap :: s -> AttrMap
cellAttrMap = const . attrMap Vty.defAttr . fmap (first getCellAttrName) $
  [ (CellSelectedAttr, bg $ Vty.rgbColor 40 40 40)
  , (CellNotSelectedAttr, bg Vty.black)
  , (CellSelectedAttr <> CellAliveAttr, fg Vty.green)
  , (CellSelectedAttr <> CellDeadAttr, fg Vty.red)
  , (CellNotSelectedAttr <> CellAliveAttr, fg Vty.brightGreen)
  , (CellNotSelectedAttr <> CellDeadAttr, fg Vty.brightRed)
  ]

-- monoid homomorphism
getCellAttrName :: CellAttr -> AttrName
getCellAttrName attr = case attr of
  CellSelectedAttr -> attrName "cell.selected"
  CellNotSelectedAttr -> attrName "cell.unselected"
  CellAliveAttr -> attrName "cell.alive"
  CellDeadAttr -> attrName "cell.dead"
  EmptyAttr -> mempty
  CombinedAttr attrA attrB -> getCellAttrName attrA <> getCellAttrName attrB

withCellAttr :: CellAttr -> Widget n -> Widget n
withCellAttr = withAttr . getCellAttrName

instance Semigroup CellAttr where
  (<>) = CombinedAttr

instance Monoid CellAttr where
  mappend = (<>)

  mempty = EmptyAttr

-- utility functions
getCellStateAttr :: BinaryCell -> CellAttr
getCellStateAttr cell = case cell of
  Full -> CellAliveAttr
  Empty -> CellDeadAttr

getCellSelectionAttr :: CellSelectionStatus -> CellAttr
getCellSelectionAttr cell = case cell of
  CellSelected -> CellSelectedAttr
  CellNotSelected -> CellNotSelectedAttr

getCellAttr :: CellSelectionStatus -> BinaryCell -> CellAttr
getCellAttr selectionStatus cell =
  getCellSelectionAttr selectionStatus <> getCellStateAttr cell

--------------------------------------------------------------------------------
-- Drawing
--------------------------------------------------------------------------------
drawCell :: CellSelectionStatus -> BinaryCell -> Widget n
drawCell selectionStatus cell = withCellAttr attr (padLeftRight 1 widget)
  where
    attr = getCellAttr selectionStatus cell

    widget = case cell of
      Empty -> str "."
      Full -> str "x"

drawGrid :: (Int, Int) -> Grid BinaryCell -> Widget n
drawGrid selectionPosition grid = vBox (fmap hBox renderedCellRows)
  where
    renderedCellRows =
      [ [ renderedCells ! (row, col) | col <- [ colBegin .. colEnd ] ]
      | row <- [ rowBegin .. rowEnd ]
      ]

    gridWithSelectionStatus = getGridWithSelectionStatus selectionPosition grid

    renderedCells = fmap (uncurry drawCell) gridWithSelectionStatus

    ((rowBegin, colBegin), (rowEnd, colEnd)) = bounds grid

getGridWithSelectionStatus
  :: (Int, Int) -> Grid BinaryCell -> Grid (CellSelectionStatus, BinaryCell)
getGridWithSelectionStatus selectionPosition = mapWithIndices getSelectionStatus
  where
    getSelectionStatus :: ((Int, Int), BinaryCell) -> (CellSelectionStatus, BinaryCell)
    getSelectionStatus (pos, cell) =
      ( if pos == selectionPosition then CellSelected else CellNotSelected
      , cell
      )
