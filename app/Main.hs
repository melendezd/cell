module Main where

import           Brick
import           Brick.BChan

import           Cell

import           Control.Concurrent ( forkIO, threadDelay )

import qualified Graphics.Vty       as Vty

title :: String
title = "cellular autamata uwu"

main :: IO ()
main = do
    runCell title (32, 32)
    putStrLn "woof"
