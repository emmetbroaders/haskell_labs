module HaskellGame.Utils where

import Prelude (
                Num(..), Ord(..), Read(..),
                Int(), String(),
                otherwise, read, (.)
               )

import qualified Data.List as List

import HaskellGame.Datatypes

{- Utility functions to do stuff we need to do -}

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
  Map w h (chunksOf w (List.map (read . (:[])) c))
