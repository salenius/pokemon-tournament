{-# LANGUAGE TemplateHaskell #-}

module Attribute.Ailment where

import Control.Lens

data Healthy' = Healthy' deriving (Eq,Show,Ord)

data Poisoned' = Poisoned' deriving (Eq,Show,Ord)

data Burned' = Burned' deriving (Eq,Show,Ord)

data Paralyzed' = Paralyzed' deriving (Eq,Show,Ord)

data Frozen' = Frozen' deriving (Eq,Show,Ord)

data Sleep' = Sleep' deriving (Eq,Show,Ord)

data BadAilment' =
  Poisoned Poisoned'
  | Burned Burned'
  | Paralyzed Paralyzed'
  | Frozen Frozen'
  | Sleep Sleep'
  deriving (Eq,Show,Ord)

data Ailment =
  Healthy Healthy'
  | BadAilment BadAilment'
  deriving (Eq,Show,Ord)

data StatusAilment =
  HealthyCondition Healthy'
  | BadStatusAilment (BadAilment', Int)
  deriving (Eq,Show,Ord)

makeClassyPrisms ''BadAilment'
makePrisms ''Ailment
makePrisms ''StatusAilment

instance AsBadAilment' Ailment where
  _BadAilment' = _BadAilment
