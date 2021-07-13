{-# LANGUAGE FlexibleInstances #-}

module Domain.Entity.GenV.Hit where

data MoveHit =
  NormalHit
  | BypassesAccuracyCheckInWeather Weather
  deriving (Eq,Show)

data Weather =
  Sunny
  | Rainy
  | Hail
  | Sandstorm
  deriving (Eq,Show,Ord,Enum)

class HitAlgebra m where
  bypassesAccuracyCheckInWeather :: Weather -> m -> m
  bypassesAccuracyCheckInRain :: m -> m
  bypassesAccuracyCheckInRain = bypassesAccuracyCheckInWeather Rainy
  bypassesAccuracyCheckInHail :: m -> m
  bypassesAccuracyCheckInHail = bypassesAccuracyCheckInWeather Hail
  bypassesAccuracyCheckInSandstorm :: m -> m
  bypassesAccuracyCheckInSandstorm = bypassesAccuracyCheckInWeather Sandstorm
  bypassesAccuracyCheckInSunny :: m -> m
  bypassesAccuracyCheckInSunny = bypassesAccuracyCheckInWeather Sunny
