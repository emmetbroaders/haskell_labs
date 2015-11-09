module HaskellGame.Utils where

import HaskellGame.Datatypes

{- Utility functions to do stuff we need to do -}

width :: Map -> Int
width (Map w _ _) = w

height :: Map -> Int
height (Map _ h _) = h

contents :: Map -> [[Tile]]
contents (Map _ _ c) = c

getPlayer :: Scene -> Object
getPlayer (Scene _ p _ _) = p

getObjects :: Scene -> [Object]
getObjects (Scene _ _ obs _) = obs

getMonsters :: Scene -> [Monster]
getMonsters (Scene _ _ _ mons) = mons

takesome :: Int -> [a] -> [a]
takesome 0 _ = []
takesome _ [] = []
takesome n (x:xs)
  | n > 0 = x:(takesome (n-1) xs)
  | otherwise = []

dropsome :: Int -> [a] -> [a]
dropsome 0 x = x
dropsome _ [] = []
dropsome n (x:xs)
  | n > 0 = (dropsome (n-1) xs)
  | otherwise = (x:xs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n l = (takesome n l):(chunksOf n (dropsome n l))

createMap :: Int -> Int -> String -> Map
createMap w h c =
  Map w h (chunksOf w $ map (read . (:[])) c)
