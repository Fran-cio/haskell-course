{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module User.Actions.Battle (battle) where

import System.Random (randomRIO)

data Golem = Golem {gAttack :: Int, gHp :: Int} deriving (Show)
data Player = Player {pAttack :: Int, pHp :: Int} deriving (Show)
data Battle = Fight | RunAway deriving (Show, Read)

battle :: IO Bool
battle = do
  pHp <- randomRIO (95, 105)
  gHp <- randomRIO (60, 80)
  gAttack <- randomRIO (10, 15)
  pAttack <- randomRIO (8, 20)
  putStrLn "\nTe encontraste un Golem, vas a correr o pelear?"
  let initialState = (Player {pAttack, pHp}, Golem gAttack gHp)
  battleLoop initialState

battleLoop :: (Player, Golem) -> IO Bool
battleLoop (Player _ hp, _) | hp <= 0 = return False
battleLoop (_, Golem _ hp) | hp <= 0 = return True
battleLoop (Player {..}, Golem {..}) = do
  putStrLn $ "El golem tiene " ++ show gHp ++ "Hp y hace " ++ show gAttack ++ " de danio."
  putStrLn $ "Vos tenes " ++ show pHp ++ "Hp y haces " ++ show pAttack ++ " de danio."
  putStrLn "\n Vas a Fight o RunAway?"
  selectedMove <- getLine
  let move = read selectedMove
  (updatedPlayer, updatedGolem) <- makeBattle (Player pAttack pHp) (Golem gAttack gHp) move
  battleLoop (updatedPlayer, updatedGolem)

makeBattle :: Player -> Golem -> Battle -> IO (Player, Golem)
makeBattle (Player pAttack pHp) (Golem gAttack gHp) Fight = do
  putStrLn "Buen ataque"
  return (Player pAttack (pHp - gAttack), Golem gAttack (gHp - pAttack))
makeBattle (Player pAttack pHp) (Golem gAttack gHp) RunAway = do
  escape <- randomRIO @Int (0, 1)
  case escape of
    0 -> do
      putStrLn "Huiste con exito"
      return (Player pAttack (pHp - gAttack), Golem gAttack 0)
    _ ->do
      putStrLn "El golem evito tu huida"
      return (Player pAttack (pHp - gAttack), Golem gAttack gHp)
