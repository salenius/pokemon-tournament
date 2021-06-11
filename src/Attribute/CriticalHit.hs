{-# LANGUAGE LambdaCase #-}

module Attribute.CriticalHit where

data CriticalHit = NotCritical | IsCritical deriving (Eq,Show,Ord,Enum)

data CriticalHitLevel =
  NeverHits
  | Level0
  | Level1
  | Level2
  | Level3
  | Level4
  | AlwaysHits
  deriving (Eq,Show,Ord,Enum)

increaseCrit :: CriticalHitLevel -> CriticalHitLevel
increaseCrit AlwaysHits = AlwaysHits
increaseCrit x = succ x

levelToInt :: CriticalHitLevel -> Int
levelToInt = \case
  NeverHits -> -1
  Level0 -> 0
  Level1 -> 1
  Level2 -> 2
  Level3 -> 3
  Level4 -> 4
  AlwaysHits -> 5

