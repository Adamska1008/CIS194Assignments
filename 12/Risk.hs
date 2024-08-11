{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List (sortBy)
import Data.Ord (Down (Down), comparing)
import GHC.Base (VecElem (Int16ElemRep))

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army}

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  let numAttackDice = min 3 (attackers bf - 1)
      numDefenceDice = min 2 (defenders bf)
  attackRolls <- replicateM numAttackDice die
  defenseRolls <- replicateM numAttackDice die
  let sortedAttack = sortBy (comparing Down) attackRolls
      sortedDefense = sortBy (comparing Down) defenseRolls
  let results = zip sortedAttack sortedDefense
      (newAttackers, newDefenders) = foldl compareDice (attackers bf, defenders bf) results

  return $ Battlefield newAttackers newDefenders
  where
    compareDice (a, d) (DV atk, DV def)
      | atk > def = (a, d - 1)
      | otherwise = (a - 1, d)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf =
  battle bf >>= \x ->
    if attackers x == 0 || defenders x == 0
      then
        return x
      else
        battle x

isSuccessful :: Battlefield -> Bool
isSuccessful bf = defenders bf == 0

successProb :: Battlefield -> Rand StdGen Double
successProb bf = (/) <$> successes <*> pure 1000.0
  where
    successes =
      replicateM 1000 (invade bf) >>= \results ->
        return $ fromIntegral $ length $ filter isSuccessful results

-- successes = length $ filter isSuccessful results
-- tot = return 1000.0
-- fails = foldl (\acc bf -> (+) <$> acc <*> fmap win bf) (return 0.0) [invade bf | _ <- [1 .. 1000]]