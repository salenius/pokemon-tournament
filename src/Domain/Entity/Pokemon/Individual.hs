module Domain.Entity.Pokemon.Individual where

import qualified Data.Map as DM

data Stat =
  MaxHP
  | Attack
  | Defence
  | SAttack
  | SDefence
  | Speed
  deriving (Eq,Show,Ord)

data IndividualFactor =
  LevelOf
  | NatureOf
  | EffortValue Stat
  | IndividualValue Stat
  deriving (Eq,Show,Ord)

data Nature =
  Timid
  deriving (Eq,Show)

newtype Statistic = Statistic {getStatistic :: Int}

statistic :: (Stat -> Int) -> (IndividualFactor -> Double) -> Stat -> Statistic
statistic = undefined

statList = [MaxHP,Attack,Defence,SAttack,SDefence,Speed]

assignEffortValues :: [Int] -> [(IndividualFactor,Int)]
assignEffortValues lst
  | length lst < 6 = assignEffortValues $ lst ++ [0]
  | otherwise = map (uncurry (\x y -> (EffortValue x, y))) . zip statList . take 6 $ lst

assignIndividualValues :: [Int] -> [(IndividualFactor,Int)]
assignIndividualValues lst
  | length lst < 6 = assignIndividualValues $ lst ++ [31]
  | otherwise = map (uncurry (\x y -> (IndividualValue x, y))) . zip statList . take 6 $ lst

defaultIndiv :: IndividualFactor -> Double
defaultIndiv LevelOf = 100
defaultIndiv NatureOf = 1.0
defaultIndiv (EffortValue _) = 0
defaultIndiv (IndividualValue _) = 31

augmentFactors :: [(IndividualFactor,Int)] -> (IndividualFactor -> Double) -> (IndividualFactor -> Double)
augmentFactors ivfs ind fct = case value of
                                Just x -> fromIntegral x
                                Nothing -> ind fct
  where
    value = DM.lookup fct (DM.fromList ivfs)
 
