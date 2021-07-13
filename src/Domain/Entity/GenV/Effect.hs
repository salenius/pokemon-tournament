{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Domain.Entity.GenV.Effect where

data family Effect a

data Battle

data instance Effect Battle =
  CauseDamage
  | ChangeWeather Weather
  | CauseRecoil Double
  | DrainDamage Double
  | CounterpartySpecific Counterparty (Effect Counterparty)
  | LockMove Int Int
  | EqualizeHPs
  | WithProbability Double (Effect Battle)
  | CallRandomMove AvailableMoves
  | NoEffect
  deriving (Eq,Show,Ord)

data instance Effect Counterparty =
  CauseAilment Ailment
  | CauseEffectWithCount Int (Effect Counterparty)
  | ThawPokemon
  | CauseFlinch
  | RaiseStat Stat Int
  | DropStat Stat Int
  | LeechSeed
  | Protect
  | Confuse Int Int
  | Screen PutOrSmash Screen
  | RecoverHP Double
  | SwitchPokemon
  deriving (Eq,Show,Ord)

data AvailableMoves =
  OwnMoves
  | AllMoves
  deriving (Eq,Show,Ord,Enum)

data Counterparty = User | Target deriving (Eq,Show,Ord,Enum)

data Weather = Weather WeatherType Int deriving (Eq,Show,Ord)

data WeatherType =
  NoWeather
  | Sunny
  | Rainy
  | Hail
  | Sandstorm
  deriving (Eq,Show,Ord,Enum)

data PutOrSmash = Put Int | Smash deriving (Eq,Show,Ord)

data Screen = LightScreen | Reflect deriving (Eq,Show,Ord,Enum)

data Stat =
  Attack
  | Defence
  | SAttack
  | SDefence
  | Speed
  | Accuracy
  | Evasion
  deriving (Eq,Show,Ord,Enum)

data Ailment =
  Healthy
  | Poison
  | BadlyPoison
  | Burn
  | Paralyze
  | Sleep Int Int
  | Freeze Int Int
  deriving (Eq,Show,Ord)

class SideEffectAlgebra m where
  causeAilment :: Counterparty -> Ailment -> m -> m
  causeEffectWithCount :: Counterparty -> Effect Counterparty -> Int -> m -> m
  causeAilmentWithCount :: Counterparty -> Ailment -> Int -> m -> m
  causeAilmentWithCount cp ai = causeEffectWithCount cp (CauseAilment ai)
  causeFlinch :: Counterparty -> m -> m
  causeRecoil :: Double -> m -> m
  drainDamage :: Double -> m -> m
  raiseStat :: Counterparty -> Stat -> Int -> m -> m
  dropStat :: Counterparty -> Stat -> Int -> m -> m
  changeWeather :: Weather -> m -> m
  leechSeedPokemon :: Counterparty -> m -> m
  protectPokemon :: Counterparty -> m -> m
  causeConfusion :: Counterparty -> Int -> Int -> m -> m
  lightScreenUp :: Counterparty -> Int -> m -> m
  reflectUp :: Counterparty -> Int -> m -> m
  breakLightScreen :: Counterparty -> m -> m
  breakReflect :: Counterparty -> m -> m
  switchPokemon :: Counterparty -> m -> m
  thaw :: Counterparty -> m -> m
  lockMove :: Int -> Int -> m -> m
  recoverHP :: Counterparty -> Double -> m -> m
  equalizeHPs :: m -> m
  callRandomMove :: AvailableMoves -> m -> m

class SideEffectCombine m where
  withProbability :: Double -> (m -> m) -> m -> m
  
