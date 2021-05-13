{-# LANGUAGE DeriveGeneric #-}

module Stats.Nature where

import GHC.Generics

data Nature =
  Hardy
  | Lonely
  | Brave
  | Adamant
  | Naughty
  | Bold
  | Docile
  | Relaxed
  | Impish
  | Lax
  | Timid
  | Hasty
  | Serious
  | Jolly
  | Naive
  | Modest
  | Mild
  | Quiet
  | Bashful
  | Rash
  | Calm
  | Gentle
  | Sassy
  | Careful
  | Quirky
  deriving (Eq,Show,Read,Enum,Ord,Generic)

