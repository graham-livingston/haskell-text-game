module Main where


import Control.Monad.State
import Control.Monad

import GameState
import GameIO


main :: IO ()

main = evalStateT gameLoop initialState

gameLoop :: GameIO ()
gameLoop = do
  opening
  forever repl
