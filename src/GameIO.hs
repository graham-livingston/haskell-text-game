module GameIO where

import Control.Monad.State
import Control.Monad
import System.Exit
import System.IO

import GameState
import Player
import Command
import Item
import Direction

type GameIO a = StateT GameState IO a

effectChange :: (GameState -> GameState) -> GameIO ()
effectChange = modify

prompt :: GameIO ()
prompt = do
  cs <- get
  let currentLoc = show (location (player cs))
  lift $ putStr (currentLoc ++ " $ ")
  lift $ hFlush stdout

printMessage :: GameIO ()
printMessage = do
  cs <- get
  case message cs of
    Nothing -> pure ()
    Just msg -> 
      lift (putStrLn msg) >> 
      put (setMessage "" cs)

-- Check if we've won
checkGameOver :: GameIO ()
checkGameOver = do
  cs <- get
  let loc = location (player cs)
  let hasSecret = SecretTxt `elem` nearbyObjects cs
  if loc == GirlfriendHome && hasSecret
    then finishGame
    else return ()

finishGame :: GameIO ()
finishGame = do
  lift $ putStrLn "You've freed your girlfriend from her encrypted file!"
  lift $ putStrLn "Congratulations! You've won!"
  lift exitSuccess

exit :: GameIO ()
exit = do
  lift $ putStrLn "Logging out..."
  lift exitSuccess

syntaxError :: GameIO ()
syntaxError = do
  lift $ putStrLn "Command not recognized."

opening :: GameIO ()
opening = do
  lift $ do
    content <- readFile "./src/ascii.txt"
    putStrLn content
  lift $ putStrLn "===== Terminal Login ====="
  lift $ putStrLn "Welcome to UnixLove OS v1.0"
  lift $ putStrLn "Your girlfriend is in an encrypted file somewhere in the system."
  lift $ putStrLn "Navigate the filesystem, gain root access, and find the SSH key to decrypt her."
  lift $ putStrLn "=========================="

performCommand :: Command -> GameIO ()
performCommand command = case command of
  Ls -> do
    cs <- get
    put $ displayRoom cs
    printMessage
  
  Cd dir -> do
    cs <- get
    put $ move dir cs
    printMessage

  Take items -> do
    cs <- get
    let newState = foldr (flip (.)) id (map takeItem items) cs
    put newState
    printMessage

  Drop items -> do
    cs <- get
    let newState = foldr (flip (.)) id (map dropItem items) cs
    put newState
    printMessage

  Run prog args -> do
    cs <- get
    put $ runProgram prog args cs
    printMessage

  Inventory -> do
    cs <- get
    put $ displayInventory cs
    printMessage

  Exit -> exit

performConjunction :: Conjunction -> GameIO ()
performConjunction commands = mapM_ performCommand commands

parseConjunction :: String -> GameIO ()
parseConjunction input = case parseInput input of
    Nothing -> syntaxError
    Just commands -> performConjunction commands

repl :: GameIO ()
repl = do
  prompt
  input <- lift getLine
  when (not $ null input) $ do
    parseConjunction input
    checkGameOver
    repl
