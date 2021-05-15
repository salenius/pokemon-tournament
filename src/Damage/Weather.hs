{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}


module Damage.Weather where

import Control.Lens
import Attribute.Weather
import Attribute.Category
import Attribute.Counterparty
import Types.BuiltIn
import Types.Pokemon
import Damage.Interpreter

data WeatherData d (m :: * -> *) = WeatherData
  {
    _currentWeather :: Weather
  , _moveType :: TypeOf
  , _pokemonType :: Counterparty -> PokemonType
  , _moveCategory :: DamagingCategory'
  , _impactOnMove :: WeatherCase
  , _impactOnSDefence :: DamagingCategory' -> Weather -> PokemonType -> Double
  , _nextCriterion :: d m
  }

type WeatherCase = TypeOf -> Weather -> Double

makeClassy ''WeatherData

class ToWeatherData a where
  toWeatherData :: DamageOps m => a -> WeatherData d m

mkWeatherData :: DamageOps m => Weather -> TypeOf -> (Counterparty -> PokemonType) -> DamagingCategory' -> d m -> WeatherData d m
mkWeatherData w tp cpt cat nxt = WeatherData w tp cpt cat weatherFn rockSandstorm nxt

instance InterpretDamage d => InterpretDamage (WeatherData d) where
  interp wd = do
    dm <- interp $ wd ^. nextCriterion
    let mt = wd ^. moveType
    let pt = (wd ^. pokemonType) Target
    let mc = wd ^. moveCategory
    let wt = wd ^. currentWeather 
    let mEff = (wd ^. impactOnMove) mt wt
    let ssEff = (wd ^. impactOnSDefence) mc wt pt
    return $ dm <> Damage mEff <> Damage ssEff


updateWeatherCase :: (TypeOf, Weather, Double) -> WeatherCase -> WeatherCase
updateWeatherCase (tp, w, v) wf = \t w' -> if (t, w') == (tp, w) then v else wf t w'

weatherFn =
  updateWeatherCase (Fire, Sunny, 1.5) .
  updateWeatherCase (Fire, Rainy, 0.5) .
  updateWeatherCase (Water, Sunny, 0.5) .
  updateWeatherCase (Water, Rainy, 1.5) $
  (\_ _ -> 1.0)

rockSandstorm :: DamagingCategory' -> Weather -> PokemonType -> Double
rockSandstorm cat w pt = case (cat, w, pt) of
  (PhysicalMove _, Sandstorm, elem Rock -> True) -> 0.5
  _ -> 1.0
