module Main (main) where

import Lib 
import Control.Concurrent (threadDelay)
import System.Console.ANSI (hideCursor)

main :: IO ()
main = run

run :: IO ()
run = do
  hideCursor
  loop blinker

loop :: Field -> IO ()
loop field = do
  let update = updateField field
  printField update
  threadDelay 500000
  loop update
