module Domain.Attribute.Category where

data Category =
  Status
  | Physical
  | Special deriving (Eq,Show,Ord)
