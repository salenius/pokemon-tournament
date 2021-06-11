{-# LANGUAGE DeriveGeneric #-}

module Types.BuiltIn where

import GHC.Generics

data TypeOf =
  Normal
  | Flying
  | Fighting
  | Fire
  | Water
  | Grass
  | Electric
  | Rock
  | Ground
  | Poison
  | Bug
  | Steel
  | Psychic
  | Ice
  | Dark
  | Ghost
  | Dragon
  | Fairy
  deriving (Eq, Show, Read, Ord, Enum, Bounded)

typeOf :: [TypeOf]
typeOf = enumFromTo minBound maxBound
