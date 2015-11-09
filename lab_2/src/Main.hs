module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO (stdin, stdout, hReady, hPutStr, hGetChar, hPutChar, hFlush, hSetBuffering, hSetEcho, Handle(), BufferMode(..))
import System.Exit (exitFailure, exitSuccess)
import qualified System.Console.ANSI as Console

import HaskellGame.Datatypes
import HaskellGame.Utils
import HaskellGame.Rendering
import HaskellGame.Interaction

{- Aliases to make it clearer what's happening in main -}

screen = stdout
keyboard = stdin
frameRate = round $ 1.0e6/24.0

theMap =
  "                    " ++
  "   ###########      " ++
  "  #...........#     " ++
  "  #............#    " ++
  "  #.............#   " ++
  "  #.............#   " ++
  "  #.............#   " ++
  "   #............#   " ++
  "    #...........#   " ++
  "     ###########    " ++
  "                    "

theObjects = [Chest (13,6)]

theMonsters = [Zombie 1 1 (6,4),
               Dragon 2 2 (7,8)]

theScene = Scene (createMap 20 10 theMap)
                 (Player (10,5))
                 theObjects
                 theMonsters

main = do
  hSetBuffering keyboard NoBuffering
  hSetBuffering screen LineBuffering
  hSetEcho screen False
  Console.hHideCursor screen
  gameScene <- newIORef $ theScene
  forever $ do
    Console.hClearScreen screen
    c <- getCharacter keyboard
    gscene <- readIORef gameScene
    let newScene = handleInput c gscene
    renderScene screen newScene

    let theDragon = head (tail (getMonsters newScene))
    let theZombie = head (getMonsters newScene)
    let theChest = head (getObjects newScene)
    let thePlayer = getPlayer newScene

    if (distance theDragon thePlayer) == 1 then do
      showEndScreen screen "CHOMP! The DRAGON has eaten you!" "GAME OVER"
      exitFailure
    else if (distance theZombie thePlayer) == 1 then do
      showEndScreen screen "CHOMP! The ZOMBIE has eaten you!" "GAME OVER"
      exitFailure  
    else if (distance theChest thePlayer) == 1 then do
      showEndScreen screen "KACHING! You plunder untold riches from the CHEST!" "YOU WIN!"
      exitSuccess
    else do
      Console.hSetCursorPosition screen 2 20
      hPutStr screen ("Distance to the CHEST: " ++ show (distance thePlayer theChest) ++ " steps.")
      Console.hSetCursorPosition screen 4 20
      hPutStr screen ("Distance to the DRAGON: " ++ show (distance thePlayer theDragon) ++ " steps.")
      hFlush screen

    writeIORef gameScene newScene
    threadDelay frameRate
  where
    getCharacter :: Handle -> IO Char
    getCharacter handle = do
      r <- hReady handle
      if r then
        hGetChar handle
      else return ' '
