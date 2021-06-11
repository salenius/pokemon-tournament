module Attribute.Charging where

data Charging =
  Charging
  | Dive
  | Dig
  | HideInShadow
  deriving (Eq,Show,Ord,Enum)
