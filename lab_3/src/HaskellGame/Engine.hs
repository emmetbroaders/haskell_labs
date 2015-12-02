module HaskellGame.Engine where

import Prelude (
                 Num(..), Show(..), Integral(..), Ord(..), Eq(..),
                 Char(), Maybe(..), Bool(..), String(), Integer(..), Int(),
                 (.)
               )

import qualified Data.List as List -- we want to be able to say "List.map"
import Data.List ((++), (\\), delete, find, filter, zip, cycle, last, head, tail, length)

import qualified System.Console.ANSI as Console

import HaskellGame.Datatypes
import HaskellGame.Battle
import HaskellGame.Interaction
import HaskellGame.Rendering
import HaskellGame.Utils

runGame :: EngineState -> EngineState
runGame engineState =
  let oldScene = scene engineState
      deadMonsters = filter ((<=0) . health) (monsters oldScene)
      aliveMonsters = (monsters oldScene) \\ deadMonsters
      deathMessages = zip (cycle [Console.White]) (List.map ((++ " dies!") . show) deadMonsters)
      -- we've removed the dead monsters, and made a list of messages saying they died

      xpGained = List.sum (List.map level deadMonsters)
      oldPlayer = player oldScene
      newPlayer = oldPlayer { experience = (experience oldPlayer) + xpGained }
      -- give the player xp for the monsters defeated

      updatedScene = oldScene { monsters = aliveMonsters,
                                player = newPlayer }
      newScene = (handleInput (keyPressed engineState) updatedScene)
      -- we handle the key the player has pressed and get the new Scene

      (newEngineState, actionMessages) = doAction (engineState { scene = newScene })
      -- we get the next engine state be based on what the player did

      oldMessages = messages (scene newEngineState)
      newMessages = (deathMessages ++ actionMessages)
      allMessages = (addMessages oldMessages newMessages)
      updatedMessages = removeOldMessages (messageLimit engineState) allMessages
      -- updatedMessages are the messages we want to be shown
  in
    -- we want to update the messages, and add 1 to the frame number
    newEngineState { frameNumber = (frameNumber engineState) + 1,
                     scene = (scene newEngineState) { messages = updatedMessages }
                   }
  where
    doAction :: EngineState -> (EngineState, [Message])
    doAction engineState =
      let theScene = scene engineState
          thePlayer = player theScene
          theObjects = objects theScene
          allMonsters = monsters theScene
          nearbyMonsters = filter ((==1) . (distance thePlayer)) allMonsters
          (Just theChest) = find ((== (13,6)) . position) theObjects
          chestDistance = distance thePlayer theChest
      in
        case (allMonsters, chestDistance) of
          ([], 1) -> (engineState { isGameOver = True }, victoryMessages)
          ([], _) -> (engineState, [])
          (_, _)  ->
            case (nearbyMonsters, chestDistance) of
              ([], 1) -> (engineState, chestClosedMessage)
              (_, _)  -> (engineState, [])
      where
        victoryMessages = [(Console.Yellow, "You kick the chest, and it springs open with a *clang*, revealing a vast hoard of treasure!"),
                           (Console.Yellow, ""),
                           (Console.Red, "G A M E     O V E R")]

        chestClosedMessage = [(Console.Cyan, "The chest makes a dull *clunk* when you kick it, but refuses to open.")]

    {-
       We want to avoid printing the same message over and over,
       so only add a message to the message list if it's different
       to the previous one.
    -}

    addMessages :: [Message] -> [Message] -> [Message]
    addMessages [] x = x
    addMessages x [] = x
    addMessages old new =
      if (last old) == (head new) then
        addMessages old (dropsome 1 new) -- just drop the new message
      else
        addMessages (old ++ [head new]) (dropsome 1 new)

    {-
       We want to make sure only as many as the message limit messages
       are displayed, so we'll remove old ones to get down to the limit.
    -}
    removeOldMessages :: Int -> [Message] -> [Message]
    removeOldMessages limit messages =
      if length messages > limit then
        dropsome ((length messages) - limit) messages
      else messages
