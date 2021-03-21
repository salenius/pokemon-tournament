{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

module Domain.Attribute.Ailment where

data Ailment =
  Healthy
  | Poisoned
  | Paralyzed
  | Burned
  | Frozen
  | Sleep
  deriving (Eq,Show,Read,Enum)

data StatusAilment =
  IsHealthy
  | IsParalyzed
  | IsBurned
  | IsPoisoned
  | IsFrozen Int
  | IsSleep Int
  deriving (Eq,Show)

getAilment :: StatusAilment -> Ailment
getAilment = \case
  IsHealthy -> Healthy
  IsParalyzed -> Paralyzed
  IsBurned -> Burned
  IsPoisoned -> Poisoned
  IsFrozen _ -> Frozen
  IsSleep _ -> Sleep

newtype PoisonMultiplier = PoisonMultiplier Int deriving (Eq,Show,Read,Num)

data Confused = NotConfused | Confused Int deriving (Eq,Show,Read,Ord)

data Flinched = NotFlinched | Flinched deriving (Eq,Show,Read,Ord)

data LeechSeeded = NotLeechSeeded | LeechSeeded deriving (Eq,Show,Read,Ord)

data Yawned = NotYawned | YawnedFirstRound | YawnedSecondRound deriving (Eq,Show,Read,Ord)

data Protected = NotProtected | Protected deriving (Eq,Show,Read,Ord)
