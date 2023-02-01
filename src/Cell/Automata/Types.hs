module Cell.Automata.Types ( BinaryCell(..), Grid(..), Neighborhood(..) ) where

import           Data.Array
import           Data.Default

data BinaryCell = Empty | Full
  deriving Show

instance Default BinaryCell where
  def = Empty

type Grid a = Array (Int, Int) a

data Neighborhood cell = Neighborhood cell (Grid cell)
    deriving Show

