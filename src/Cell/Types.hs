module Cell.Types
  ( BinaryCell(..)
  , Grid(..)
  , AppState(..)
  , UIState(..)
  , CellApp(..)
  , appGrid
  , appState
  , cursorPos
  ) where

import           Cell.Automata.Types

import           Control.Lens.TH


newtype AppState = AppState { _appGrid :: Grid BinaryCell }
makeLenses ''AppState

newtype UIState = UIState { _cursorPos :: (Int, Int) }
makeLenses ''UIState

data CellApp = CellApp { _appState :: AppState, _uiState :: UIState }
makeLenses ''CellApp
