{-# LANGUAGE ViewPatterns #-}

module Domain.Entity.GenV.Damage where

import Domain.Entity.GenV.TypeOf
import Domain.Entity.GenV.Ability
import Domain.Entity.GenV.Item
import Data.Map as Map

data DamageLogic count =
  DamageCalc (Damage count)
  | ConstantDamageCalc (ConstantDamage count)
  | MultistrikeMove (Map Int Double) (Damage count) 
  deriving (Eq,Show,Ord)

data Damage count =
  BasepowerCalc Basepower (Damage count)
  | RatioCalc Ratio (Damage count)
  | TargetCount count (Damage count)
  | CriticalHitCalc CriticalHit (Damage count)
  | AffectedByWeather WeatherEff (Damage count)
  | NormalDamage
  deriving (Eq,Show,Ord)

data Ratio = PhysicalRatio | SpecialRatio deriving (Eq,Show,Ord,Enum)

data Basepower =
  MoveBasepower
  | MultiplyIfCondition Double Condition
  | DetermineBy Factor
  deriving (Eq,Show,Ord)

data Condition =
  UserMovedLast
  | UserReceivedDamage
  | TargetChoseDamagingMove
  | TargetUsedItem
  | TargetSwitchesOut
  | UserHasNoItem
  | Or Condition Condition
  | And Condition Condition
  deriving (Eq,Show,Ord)

data Factor = Factor Attribute (Double -> Int)

instance Show Factor where
  show (Factor a f) = "Factor " ++ show a ++ " fn[Double -> Int]"

instance Eq Factor where
  (Factor a f) == (Factor b g) = a == b

instance Ord Factor where
  (Factor a f) <= (Factor b g) = a <= b

data Attribute =
  UserHas PokemonAttr
  | TargetHas PokemonAttr
  deriving (Eq,Show,Ord)

data PokemonAttr =
  HPPercent
  | Weight
  deriving (Eq,Show,Ord)

data ConstantDamage count =
  OHKO
  | ConstantDamage Int
  | MultipleOfPreviousDamage Double 
  deriving (Eq,Show,Ord)

data CriticalHit =
  IncreasedCriticalHit Int
  | NormalCriticalHit
  deriving (Eq,Show,Ord)

data WeatherEff =
  HurtBySandstorm
  | HurtByHail
  | HurtByRain
  deriving (Eq,Show,Ord)

class DamageAlgebra m where
  multiplyBasepowerIf :: Condition -> Double -> m -> m
  determineBasepowerBy :: Attribute -> (Double -> Int) -> m -> m
  increasedCriticalHit :: Int -> m -> m
  multiStrikeMove :: [(Int,Double)] -> m -> m
  ohko :: m -> m
  constantDamage :: Int -> m -> m
  multipleOfPreviousDamage :: Double -> m -> m
  hurtBySandstorm :: m -> m
  hurtByHail :: m -> m
  hurtByRain :: m -> m
