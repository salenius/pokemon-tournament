{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Damage.Power where

import Types.BuiltIn
import Types.Effect
import Types.Pokemon
import Attribute.Counterparty
import Attribute.Weather
import Stats.Base
import Control.Lens
import Damage.Interpreter
import Damage.CriticalHit as Cr
import Damage.Ratio as R


type BasicDamageCalc a m =
  (a -> m Int)
  -> (a -> m CriticalHit)
  -> (CriticalHit -> a -> Double)
  -> (CriticalHit -> a -> m Double)
  -> a
  -> m Damage

basicDamageCalc :: DamageOps m => BasicDamageCalc a m
basicDamageCalc = undefined


data Basic a m = Basic
  {
    _basepower :: a -> m Int
  , _criticalHitSuccess :: CriticalHitCalc a m
  , _calcRatio :: Ratio a
  , _criticalHitDamage :: CriticalHit -> a -> m Double
  }

makeClassy ''Basic

instance InterpretDamage Basic where
  interp bsic =
    basicDamageCalc (view basepower bsic)
    (Cr.reduce $ view criticalHitSuccess bsic)
    (R.reduce $ view calcRatio bsic)
    (view criticalHitDamage bsic)
