module Attribute.Gender where

data Gender =
  Male
  | Female
  | Genderless
  deriving (Eq,Show,Read,Enum,Ord)
