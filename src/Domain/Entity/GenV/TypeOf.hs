{-# LANGUAGE FlexibleInstances #-}

module Domain.Entity.GenV.TypeOf where

data TypeOf =
  Normal
  | Fighting
  | Flying
  | Water
  | Fire
  | Grass
  | Electric
  | Poison
  | Bug
  | Rock
  | Ground
  | Psychic
  | Ice
  | Steel
  | Dark
  | Ghost
  | Dragon
  deriving (Eq,Show,Ord,Enum,Bounded)
