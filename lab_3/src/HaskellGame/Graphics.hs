module HaskellGame.Graphics where

import Prelude ( Show(..) )
import qualified Data.List as List
import Data.List ((++))
import Data.Char (toUpper)

import HaskellGame.Datatypes
import HaskellGame.Utils

{- How to display active game elements -}

instance Show Player where
  show (Player _ _ _ _ _) = "☃"

instance Show Object where
  show (Chest _) = "?"

instance Show Monster where
  show (Dragon _ _ _) = "🐉"
  show (Zombie _ _ _) = "💀"

{- Displaying stats and status -}

instance Show Stat where
  show stat = "STAT!?" -- Task 1 - fix this
