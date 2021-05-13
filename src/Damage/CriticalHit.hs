{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}


module Damage.CriticalHit where

import Damage.Interpreter
import Control.Lens

data CriticalHit = NotCritical | IsCritical deriving (Eq,Show,Ord,Enum)

data CriticalHitLevel =
  Level0
  | Level1
  | Level2
  | Level3
  | Level4
  deriving (Eq,Show,Ord,Enum)

levelToInt :: CriticalHitLevel -> Int
levelToInt = \case
  Level0 -> 0
  Level1 -> 1
  Level2 -> 2
  Level3 -> 3
  Level4 -> 4

type Prob = Double

data CriticalHitCalc a m = CriticalHitCalc
  {
    _initialLevel :: CriticalHitLevel
  , _levelIntToProb :: Int -> Prob
  , _drawHit :: a -> Prob -> m Double
  }

makeLenses ''CriticalHitCalc

reduce :: DamageOps m => CriticalHitCalc a m -> a -> m CriticalHit
reduce calc dta = do
  v <- drawRandomDouble
  let lvl = view levelIntToProb calc . levelToInt $ view initialLevel calc
  h <- view drawHit calc dta lvl
  let res = case (v < h) of
              True  -> IsCritical
              False -> NotCritical
  return res
    
