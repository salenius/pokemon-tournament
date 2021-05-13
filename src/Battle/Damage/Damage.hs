{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}


module Battle.Damage.Damage where

import Types.BuiltIn
import Types.Effect
import Types.Pokemon
import Attribute.Counterparty
import Attribute.Weather
import Stats.Base
import Control.Lens

type CriticalHit = Bool
type Damage = Double

type BasicDamageCalc a m =
  (a -> m Int)
  -> (a -> m CriticalHit)
  -> (a -> Counterparty -> ModifStat -> Double)
  -> (a -> CriticalHit -> m (Double,Double))
  -> (CriticalHit -> m Double)
  -> a
  -> m Damage

data Basic a m = Basic
  {
    _basepower :: a -> m Int
  , _criticalHitSuccess :: a -> m CriticalHit
  , _ratioOf :: Ratio a
  , _
  }

type TypeAdvantageDamage a m =
  (TypeOf -> TypeOf -> TypeEffect)
  -> (a -> Counterparty -> PokemonType)
  -> (a -> TypeOf)
  -> (TypeEffect -> Double)
  -> (Double -> TypeEffect)
  -> (TypeEffect -> m Double)
  -> (a -> Counterparty -> Maybe SupereffectiveBerry)
  -> (a -> Counterparty -> Maybe ExpertBelt)
  -> (a -> m Damage)
  -> a -> m Damage

type WeatherDamage a m =
  (Weather -> TypeOf -> Double)
  -> (a -> TypeOf)
  -> (Weather -> PokemonType -> Double)
  -> (a -> Weather)
  -> (a -> m Damage)
  -> a -> m Damage
  
