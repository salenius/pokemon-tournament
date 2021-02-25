module Domain.Attribute.Conditions where

data PokemonCondition =
  LeechSeeded
  | Flinched
  | Yawned
  | Confused
  | Protected
  deriving (Eq,Show)

data PlayerCondition =
  LightScreen
  | Reflect
  | Safeguard
  deriving (Eq,Show)
