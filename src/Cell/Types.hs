module Cell.Types
  ( BinaryCell(..)
  , Grid(..)
  , AppState(..)
  , UIState(..)
  , CellApp(..)
  , AppStatus(..)
  , appGrid
  , appState
  , appStatus
  , uiState
  , cursorPos
  ) where

import           Cell.Automata.Types

import           Control.Lens.TH


data AppStatus = AppRunning | AppPaused
makeLenses ''AppStatus

newtype AppState = AppState { _appGrid :: Grid BinaryCell }
makeLenses ''AppState

newtype UIState = UIState { _cursorPos :: (Int, Int) }
makeLenses ''UIState

data CellApp = CellApp { _appState :: AppState, _uiState :: UIState, _appStatus :: AppStatus }
makeLenses ''CellApp
