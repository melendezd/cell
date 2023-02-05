module Cell.Util (mapWithIndices) where

import           Data.Array.IArray

mapWithIndices :: (IArray a e1, IArray a e2, Ix i) => ((i, e1) -> e2) -> a i e1 -> a i e2
mapWithIndices f arr = array (bounds arr) (fmap fAndIndex (assocs arr))
  where
    fAndIndex (j, x) = (j, f (j, x))
