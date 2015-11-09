module HaskellGame.Datatypes where

{- Data types -}

data Tile = Grass
          | Wall
          | Empty

data Map = Map Int Int [[Tile]]

type Point = (Int, Int)

data Object = Player Point
            | Chest Point

data Monster = Dragon Int Int Point
             | Zombie Int Int Point

data Scene = Scene Map Object [Object] [Monster]

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
  position (Player pt) = pt
  position (Chest pt) = pt

instance Located Monster where
  position (Dragon _ _ pt) = pt
  position (Zombie _ _ pt) = pt

{- How to read and display the game map -}

instance Read Tile where
  readsPrec _ "." = [(Grass, "")]
  readsPrec _ "#" = [(Wall, "")]
  readsPrec _ _   = [(Empty, "")]

instance Show Tile where
  show Grass  = "."
  show Wall   = "#"
  show Empty  = " "
