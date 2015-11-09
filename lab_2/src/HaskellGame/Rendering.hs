module HaskellGame.Rendering where

import Control.Concurrent (threadDelay)
import System.IO (hPutStr, hFlush, Handle())
import qualified System.Console.ANSI as Console

import HaskellGame.Datatypes
import HaskellGame.Utils

{- How to read and display active game elements -}

instance Show Object where
  show (Player _) = "☃"
  show (Chest _) = "?"

instance Show Monster where
  show (Dragon _ _ _) = "🐉"
  show (Zombie _ _ _) = "💀"

{- Rendering the game world to the console -}

renderMap :: Handle -> Map -> IO ()
renderMap theScreen theMap = do
  let xcoords = [0..((width theMap)-1)]
  let ycoords = [0..((height theMap)-1)]
  let allCoords = concatMap (\ycoord -> zip xcoords $ replicate (width theMap) ycoord) ycoords
  mapM_ (drawItem theMap theScreen) allCoords
  where
    drawItem theMap theScreen (x,y) = do
      let item = (((contents theMap) !! y) !! x)
      Console.hSetCursorPosition theScreen y x
      hPutStr theScreen (show item)

render :: (Show a, Located a) => Handle -> a -> IO ()
render theScreen obj = do
  let (x, y) = position obj
  Console.hSetCursorPosition theScreen y x
  hPutStr theScreen (show obj)

renderScene :: Handle -> Scene -> IO ()
renderScene theScreen (Scene map player objects monsters) = do
  renderMap theScreen map
  mapM_ (render theScreen) objects
  mapM_ (render theScreen) monsters
  render theScreen player

{- Print a nice win or lose screen -}

showEndScreen theScreen message1 message2 = do
  Console.hSetCursorPosition theScreen 2 20
  Console.hSetSGR theScreen [Console.SetColor Console.Foreground Console.Vivid Console.Yellow]
  hPutStr theScreen message1
  Console.hSetCursorPosition theScreen 4 30
  Console.hSetSGR theScreen [Console.SetColor Console.Foreground Console.Vivid Console.Red]
  hPutStr theScreen message2
  Console.hSetSGR theScreen [Console.SetColor Console.Foreground Console.Dull Console.White]
  hFlush theScreen
  threadDelay (round 2e6)
  Console.hSetCursorPosition theScreen 0 0
  Console.hShowCursor theScreen
  Console.hClearScreen theScreen
  hFlush theScreen
