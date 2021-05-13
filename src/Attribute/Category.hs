{-# LANGUAGE TemplateHaskell #-}

module Attribute.Category where

import Control.Lens

data DamagingCategory' =
  PhysicalMove Physical
  | SpecialMove Special
  deriving (Eq,Show,Ord)

data MoveCategory =
  StatusMove Status
  | DamagingCategory DamagingCategory'
  deriving (Eq,Show,Ord)

data Physical = Physical deriving (Eq,Show,Ord)

data Special = Special deriving (Eq,Show,Ord)

data Status = Status deriving (Eq,Show,Ord)

makePrisms ''DamagingCategory'
makePrisms ''MoveCategory
