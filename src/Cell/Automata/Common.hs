module Cell.Automata.Common
  ( newGrid
  , (!%)
  , mooreNeighborhood
  , numAlive
  , numDead
  ) where

import           Cell.Automata.Types

import           Data.Array
import           Data.Default

newGrid :: (Default cell) => (Int, Int) -> Grid cell
newGrid (numRows, numCols) =
  listArray ((0, 0), (numRows - 1, numCols - 1)) (repeat def)

-- Index a grid with wrap-around
(!%) :: (Integral i, Ix i) => Array (i, i) a -> (i, i) -> a
arr !% (row, col) = arr ! (row `mod` numRows, col `mod` numCols)
  where
    ((rowBegin, colBegin), (rowEnd, colEnd)) = bounds arr

    (numRows, numCols) = (rowEnd - rowBegin + 1, colEnd - colBegin + 1)

-- 3x3 square neighborhood centered at a cell
mooreNeighborhood :: Grid cell -> (Int, Int) -> Neighborhood cell
mooreNeighborhood grid centerIdx@(row, col) = Neighborhood center neighbors
  where
    center = grid ! centerIdx

    neighborAssocs =
      [ ((r, c), grid !% (row + r, col + c))
      | r <- [ -1, 0, 1 ]
      , c <- [ -1, 0, 1 ]
      ]

    neighbors = array ((-1, -1), (1, 1)) neighborAssocs

numAlive :: Grid BinaryCell -> Int
numAlive = sum . map indicateAlive . elems
  where
    indicateAlive cell = case cell of
      Empty -> 0
      Full -> 1

numDead :: Grid BinaryCell -> Int
numDead = sum . map indicateDead . elems
  where
    indicateDead cell = case cell of
      Empty -> 1
      Full -> 0
