module Cell.Automata.Conway (evolveGridConway) where

import Data.Array

import Cell.Automata.Types
import Cell.Automata.Common

evolveCellConway :: Neighborhood BinaryCell -> BinaryCell
evolveCellConway (Neighborhood center neighbors) =
  let nAlive = numAlive neighbors
      nDead = numDead neighbors
  in
    case center of
      Empty
        | nAlive == 3 -> Full
        | otherwise -> Empty
      Full
        | nAlive < 3 -> Empty
        | nAlive > 4 -> Empty
        | otherwise -> Full

evolveGridConway :: Grid BinaryCell -> Grid BinaryCell
evolveGridConway grid = grid // updates
  where
    updates =
      fmap (\idx -> (idx, evolveCellConway . mooreNeighborhood grid $ idx))
           (indices grid)
