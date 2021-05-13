{-# LANGUAGE DeriveGeneric #-}

module Trainer.BuiltIn where

import GHC.Generics

data Trainer =
  Red
  | Blue
  | Lance
  | Steven
  | Wallace
  | Cynthia
  | Alder
  deriving (Eq,Show,Read,Generic)
