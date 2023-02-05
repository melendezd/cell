{-# LANGUAGE TupleSections #-}

module Cell ( runCell ) where

import           Brick
import           Brick.BChan

import           Cell.Automata
import           Cell.Types
import           Cell.UI

import           Control.Concurrent ( forkIO, threadDelay )
import           Control.Lens
import           Control.Monad

import           Data.Array
import qualified Data.Map as M

import qualified Graphics.Vty       as Vty

ticksPerSec, usDelay :: Int
ticksPerSec = 10

usDelay = 1000000 `div` ticksPerSec

cellAppDraw :: String -> CellApp -> [Widget Name]
cellAppDraw title app = singleton . displayGrid title (app ^. uiState . cursorPos)
  . view (appState . appGrid) $ app
  where
    singleton x = [ x ]

cellApp :: String -> App CellApp Tick Name
cellApp title =
  App { appDraw         = cellAppDraw title
      , appChooseCursor = neverShowCursor
      , appHandleEvent  = handleEvent
      , appStartEvent   = pure ()
      , appAttrMap      = cellAttrMap
      }

runCell :: String -> (Int, Int) -> IO CellApp
runCell title gridSize = do
  eventChan <- Brick.BChan.newBChan 10
  let buildVty = Vty.mkVty Vty.defaultConfig
  initialVty <- buildVty
  void . forkIO . forever $ do
    writeBChan eventChan Tick
    threadDelay usDelay

  let initialGrid = newGrid gridSize
        // fmap (, Full) [ (10, 10), (11, 10), (12, 10), (12, 9), (11, 8) ]
        // fmap (\(r, c) -> ((r + 5, c + 5), Full))
                [ (10, 10), (11, 10), (12, 10), (12, 9), (11, 8) ]
  let initialAppState = AppState { _appGrid = initialGrid }
  let initialUIState = UIState { _cursorPos = (0, 0) }
  let initialState =
          CellApp { _appState = initialAppState, _uiState = initialUIState, _appStatus = AppPaused }

  customMain initialVty buildVty (Just eventChan) (cellApp title) initialState

handleEvent :: BrickEvent Name Tick -> EventM Name CellApp ()
handleEvent event = case event of
  (AppEvent Tick) -> tick
  (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) -> halt
  (VtyEvent (Vty.EvKey key [])) -> modifying id $ maybe id handleInput (M.lookup key defaultKeyMap)
  _ -> pure ()

tick :: EventM Name CellApp ()
tick = do
  appStatus <- use appStatus
  case appStatus of
    AppRunning -> modifying (appState . appGrid) evolveGridConway
    AppPaused -> pure ()
