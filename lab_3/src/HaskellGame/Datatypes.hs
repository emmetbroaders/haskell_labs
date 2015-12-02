module HaskellGame.Datatypes where

import Prelude (
                Read(..), Show(..), Eq(..), Num(..),  -- we need these type classes out of the Prelude
                Int(), Integer(), String(), Bool(..), Char()     -- we need these data types
               )

import qualified System.Console.ANSI as Console
import System.IO (Handle())

{- Data types -}

data Tile = Grass
          | Wall
          | Empty

data Map = Map {
                 width :: Int,
                 height :: Int,
                 contents :: [[Tile]]
               }

type Point = (Int, Int)

data Object = Chest Point

data Monster = Dragon Int Int Point
             | Zombie Int Int Point
             deriving Eq

type Skill = (String, Int)

type Stat = (String, Int)

data Player = Player {
                       hitpoints :: Int,
                       experience :: Int,
                       stats :: [Stat],
                       skills :: [Skill],
                       pos :: Point
                     }

data Scene = Scene {
                     map :: Map,
                     player :: Player,
                     objects :: [Object],
                     monsters :: [Monster],
                     messages :: [Message]
                   }

-- We want nice colourful messages, so for each Message,
-- we have a colour as well as the string to print out.
type Message = (Console.Color, String)

data EngineState = EngineState {
                                 screen :: Handle,
                                 keyboard :: Handle,
                                 frameRate :: Integer,
                                 frameNumber :: Integer,
                                 keyPressed :: Char,
                                 isGameOver :: Bool,
                                 messageLimit :: Int,
                                 scene :: Scene
                               }

{- Type classes -}

class Located a where
  position :: a -> Point
  distance :: Located b => a -> b -> Int

  distance a b =
    let (x1, y1) = position a
        (x2, y2) = position b
    in abs(x1 - x2) + abs(y1 - y2)

{- Type class instances -}

instance Located Object where
  position (Chest pt) = pt

instance Located Monster where
  position (Dragon _ _ pt) = pt
  position (Zombie _ _ pt) = pt

instance Located Player where
  position (Player _ _ _ _ pt) = pt

{- How to read and display the game map -}

instance Read Tile where
  readsPrec _ "." = [(Grass, "")]
  readsPrec _ "#" = [(Wall, "")]
  readsPrec _ _   = [(Empty, "")]

instance Show Tile where
  show Grass  = "."
  show Wall   = "#"
  show Empty  = " "
