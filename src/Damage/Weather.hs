{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}


module Damage.Weather where

import Control.Lens
import Attribute.Weather
import Attribute.Category
import Attribute.Counterparty
import Types.BuiltIn
import Types.Pokemon
import Damage.Interpreter

data WeatherData d a (m :: * -> *) = WeatherData
  {
    _currentWeather :: a -> Weather
  , _moveType :: a -> TypeOf
  , _pokemonType :: a -> Counterparty -> PokemonType
  , _moveCategory :: a -> DamagingCategory'
  , _impactOnMove :: TypeOf -> Weather -> Double
  , _impactOnSDefence :: DamagingCategory' -> Weather -> PokemonType -> Double
  , _nextCriterion :: d a m
  }

makeClassy ''WeatherData

mkWeatherData ::
  DamageOps m =>
  (a -> Weather)
  -> (a -> TypeOf)
  -> (a -> Counterparty -> PokemonType)
  -> (a -> DamagingCategory')
  -> d a m
  -> WeatherData d a m
mkWeatherData wfn tpfn pktfn cfn nxt =
  WeatherData wfn tpfn pktfn cfn f g nxt
  where
    f Fire Sunny  = 1.5
    f Fire Rainy  = 0.5
    f Water Sunny = 0.5
    f Water Rainy = 0.5
    f _ _         = 1.0
    g (SpecialMove _) Sandstorm tps
      | Rock `elem` tps = 0.5
      | otherwise       = 1.0
    g _ _ _       = 1.0


instance InterpretDamage d => InterpretDamage (WeatherData d) where
  interp wd dta = do
    dm <- flip interp dta $ wd ^. nextCriterion
    let mt = (wd ^. moveType) dta
    let pt = (wd ^. pokemonType) dta Target
    let mc = (wd ^. moveCategory) dta
    let wt = (wd ^. currentWeather) dta
    let mEff = (wd ^. impactOnMove) mt wt
    let ssEff = (wd ^. impactOnSDefence) mc wt pt
    return $ dm <> Damage mEff <> Damage ssEff
