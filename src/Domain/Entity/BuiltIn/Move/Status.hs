{-# LANGUAGE TemplateHaskell #-}

module Domain.Entity.BuiltIn.Move.Status where

import Control.Lens

data Move =
  AcidArmor
  | LeechSeed
  | LightScreen
  | PainSplit
  | QuiverDance
  | Protect
  | RainDance
  | Reflect
  | Rest
  | Sandstorm
  | SleepPowder
  | SleepTalk
  | SwordsDance
  | Toxic
  | WillOWisp
  | Yawn
  deriving (Eq,Show,Read,Ord,Enum)

makePrisms ''Move
