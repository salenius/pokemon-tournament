{-# LANGUAGE FlexibleInstances #-}

module Domain.Entity.GenV.Success where

data MoveSuccess =
  NormalSuccess
  | OnlyUsedOnFirstTurn 
  | FailsIfUserFaster 
  | ChargesFirstTurn MoveCharge 
  | FailsIfTargetHasntAttacked 
  | FailsIfTargetAilmented Ailment 
  | FailsIfUserAilmented Ailment
  | DroppingSuccessRate Double
  | FailsIfTargetNotAttacking
  deriving (Eq,Show,Ord)

class SuccessAlgebra a where
  onlyUsedOnFirstTurn :: a -> a
  failsIfUserFaster :: a -> a
  chargesFirstTurn :: MoveCharge -> a -> a
  failsIfTargetHasntAttacked :: a -> a
  failsIfTargetAilmented :: Ailment -> a -> a
  failsIfUserAilmented :: Ailment -> a -> a
  droppingSuccessRate :: Double -> a -> a
  failsIfTargetNotAttacking :: a -> a

data MoveCharge =
  ChargesOneTurn
  | AttacksIfWeather Weather
  deriving (Eq,Show,Ord)

data Ailment =
  Healthy
  | Poisoned
  | Paralyzed
  | Sleep
  | Frozen
  | Burned
  deriving (Eq,Show,Ord,Enum)

data Weather =
  NoWeather
  | Rainy
  | Sunny
  | Sandstorm
  | Hail
  deriving (Eq,Show,Ord,Enum)
