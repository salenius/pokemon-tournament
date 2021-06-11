module Attribute.Ailment where

data Ailment =
  Poisoned
  | Burned
  | Paralyzed
  | Frozen
  | Sleep
  | Healthy
  deriving (Eq,Show,Ord,Enum,Bounded)

ailment :: [Ailment]
ailment = enumFromTo minBound maxBound
