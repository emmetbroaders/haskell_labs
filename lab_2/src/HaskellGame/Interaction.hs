module HaskellGame.Interaction where

import HaskellGame.Datatypes

{-
  Check if the player's new position would collide with something.
  Return True if there would be a collision.
  Return False if there would be no collision.

  Lab 2: Implement this function for 20 marks
-}

detectCollision :: Scene -> Point -> Bool
detectCollision _ (x, y) = False


{- Handle a key press from the player -}

handleInput :: Char -> Scene -> Scene
handleInput c theScene@(Scene map player objects monsters) =
  let newPosition = move player c
  in
    if detectCollision theScene newPosition then
      -- if there would be a collision, we'll just give back the old scene
      theScene
    else
      -- if there would be NO collision, we move the player
      (Scene map (Player newPosition) objects monsters)
  where
    move (Player (x, y)) chr =
      case c of
        'i' -> (x, (y-1))
        'j' -> ((x-1), y)
        'k' -> (x, (y+1))
        'l' -> ((x+1), y)
        _   -> (x, y)
