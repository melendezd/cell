{-# LANGUAGE LambdaCase #-}

module Cell.UI.InputHandler ( handleInput, defaultKeyMap ) where

import           Cell.Types
import           Cell.UI.Types

import           Control.Lens    hiding ( Empty )

import           Data.Array
import           Data.Array.Lens
import           Data.Bifunctor  ( first, second )
import           Data.Map

import qualified Graphics.Vty    as Vty

defaultKeyMap :: Map Vty.Key KeyInput
defaultKeyMap = fromList . fmap (first Vty.KChar) $
  [ ('h', KeyLeft)
  , ('j', KeyDown)
  , ('k', KeyUp)
  , ('l', KeyRight)
  , ('f', KeyToggleCell)
  , (' ', KeyToggleSimulation)
  , ('q', KeyExit)
  ]

handleInput :: KeyInput -> CellApp -> CellApp
handleInput key app = modifier app
  where
    ((rowMin, colMin), (rowMax, colMax)) = bounds (app ^. appState . appGrid)

    withinBounds :: (Int, Int) -> (Int, Int)
    withinBounds = bimap (max rowMin . min rowMax) (max colMin . min colMax)

    changeCursorPosBy :: ((Int, Int) -> (Int, Int)) -> CellApp -> CellApp
    changeCursorPosBy f = over (uiState . cursorPos) (withinBounds . f)

    toggleCellAt :: (Int, Int) -> CellApp -> CellApp
    toggleCellAt pos = over (appState . appGrid . ix pos) $ \case
      Full -> Empty
      Empty -> Full

    togglePaused :: CellApp -> CellApp
    togglePaused = over appStatus $ \case
      AppRunning -> AppPaused
      AppPaused -> AppRunning

    add = (+)

    modifier :: CellApp -> CellApp
    modifier = case key of
      KeyUp -> changeCursorPosBy (first (subtract 1))
      KeyDown -> changeCursorPosBy (first (add 1))
      KeyLeft -> changeCursorPosBy (second (subtract 1))
      KeyRight -> changeCursorPosBy (second (add 1))
      KeyToggleCell -> toggleCellAt (app ^. uiState . cursorPos)
      KeyToggleSimulation -> togglePaused
      KeyExit -> undefined

