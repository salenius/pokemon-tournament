{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}


module Attribute.Weather (
  Weather(..),
  Sunny(..),
  Rainy(..),
  Hail(..),
  Sandstorm(..),
  _Sunny,
  _Rainy,
  _Hail,
  _Sandstorm
                         ) where

import Attribute.Env

data Weather =
  Normal
  | Sunny
  | Rainy
  | Hail
  | Sandstorm
  deriving (Eq,Show,Read,Ord,Enum)

data Sunny = IsSunny deriving (Eq,Show,Read,Ord)

data Rainy = IsRainy deriving (Eq,Show,Read,Ord)

data Hail = IsHail deriving (Eq,Show,Read,Ord)

data Sandstorm = IsSandstorm deriving (Eq,Show,Read,Ord)

_Sunny :: Prism' Weather Sunny
_Sunny = mkWeather Sunny IsSunny
  
_Rainy :: Prism' Weather Rainy
_Rainy = mkWeather Rainy IsRainy

_Hail :: Prism' Weather Hail
_Hail = mkWeather Hail IsHail

_Sandstorm :: Prism' Weather Sandstorm
_Sandstorm = mkWeather Sandstorm IsSandstorm

mkWeather :: Weather -> a -> Prism' Weather a
mkWeather w a = prism' (const w) (\case
                                     ((==) w -> True) -> Just a
                                     _                -> Nothing)
