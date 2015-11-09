{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Test.HUnit
import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import HaskellGame.Datatypes
import HaskellGame.Utils
import HaskellGame.Interaction

deriving instance Show Scene
deriving instance Show Map
deriving instance Show Monster
deriving instance Show Object

deriving instance Eq Scene
deriving instance Eq Map
deriving instance Eq Monster
deriving instance Eq Object
deriving instance Eq Tile

{- HUnit Tests -}

test_no_collide_walls =
  let theMap = "###" ++
               "#.#" ++
               "###"
      theScene = (Scene (createMap 3 3 theMap) (Player (1,1)) [] [])
  in
    theScene @=? (handleInput 'j' theScene)


test_no_collide_chest =
  let theMap = "###" ++
               "#.#" ++
               "#.#" ++
               "###"
      theScene = (Scene (createMap 3 4 theMap) (Player (1,1)) [Chest (1,2)] [])
  in
    theScene @=? (handleInput 'k' theScene)

test_no_collide_zombie =
  let theMap = "###" ++
               "#.#" ++
               "#.#" ++
               "###"
      theScene = (Scene (createMap 3 4 theMap) (Player (1,1)) [] [Zombie 0 0 (1,2)])
  in
    theScene @=? (handleInput 'k' theScene)

test_no_collide_dragon =
  let theMap = "###" ++
               "#.#" ++
               "#.#" ++
               "###"
      theScene = (Scene (createMap 3 4 theMap) (Player (1,1)) [] [Dragon 0 0 (1,2)])
  in
    theScene @=? (handleInput 'k' theScene)

test_movement =
  let theMap = "###" ++
               "#.#" ++
               "#.#" ++
               "###"
      theScene = (Scene (createMap 3 4 theMap) (Player (1,1)) [] [])
      expectedScene = (Scene (createMap 3 4 theMap) (Player (1,2)) [] [])
  in
    expectedScene @=? (handleInput 'k' theScene)

{- QuickCheck Tests -}

prop_takesome_take :: Int -> Bool
prop_takesome_take n =
  let xs = [0..9]
  in takesome n xs == take n xs

prop_dropsome_drop :: Int -> Bool
prop_dropsome_drop n =
  let xs = [0..9]
  in dropsome n xs == drop n xs

main = defaultMain tests

tests :: [TF.Test]
tests = [
          testGroup "Test Collisions (Lab02)" [
            testCase "Wall Collision [5 marks]" test_no_collide_walls
          , testCase "Chest Collision [5 marks]" test_no_collide_chest
          , testCase "Zombie Collision [5 marks]" test_no_collide_zombie
          , testCase "Dragon Collision [5 marks]" test_no_collide_dragon
          ],

          -- regression tests, ot ensure we haven't broken anything previous.
          testGroup "Test Movement (Lab01)" [
            testCase "Player Movement" test_movement
          ],

          testGroup "takesome and dropsome" [
            testProperty "takesome works like Prelude.take" prop_takesome_take
          , testProperty "dropsome works like Prelude.drop" prop_dropsome_drop
          ]
        ]
