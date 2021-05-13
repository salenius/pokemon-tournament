{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Damage.Random (
  mkRandomFactor
  ,RandomFactor()
  ,randomNumber
  ,nextCondition
                     ) where

import Control.Lens
import Damage.Interpreter

data RandomFactor d a m = RandomFactor
  {
    _randomNumber :: m Double
  , _nextCondition :: d a m}

makeClassy ''RandomFactor

mkRandomFactor :: DamageOps m => d a m -> RandomFactor d a m
mkRandomFactor = RandomFactor f
  where
    f = do
      x    <- drawRandomDouble
      let y = 0.15 * x + 0.85
      return y

instance InterpretDamage d => InterpretDamage (RandomFactor d) where
  interp rf dta = do
    nxt <- flip interp dta $ rf ^. nextCondition
    rnd <- (rf ^. randomNumber)
    return $ nxt <> Damage rnd
