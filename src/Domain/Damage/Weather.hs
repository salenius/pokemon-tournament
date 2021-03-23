{-# LANGUAGE TemplateHaskell #-}

module Domain.Damage.Weather where

import Control.Lens
import Data.Functor.Contravariant
import Domain.Attribute.Weather
import Domain.Attribute.TypeOf
import Domain.Attribute.Category
import Domain.Attribute.Counterparty
import Domain.Attribute.Damage
import Domain.Pokemon.Species

data WeatherDamage battle = WeatherDamage
  {
    _typeOfMove :: battle -> TypeOf
  , _currentWeather :: battle -> Weather
  , _categoryOfMove :: battle -> Category
  , _typeOfPokemon :: Counterparty -> battle -> TypeOfPokemon
  , _effectOfType :: Weather -> TypeOf -> Double
  , _effectOfSandstorm :: Weather -> TypeOfPokemon -> Category -> Double
  }

makeLenses ''WeatherDamage

instance Contravariant WeatherDamage where
  contramap f w =
    w {_typeOfMove = _typeOfMove w . f,
       _currentWeather = _currentWeather w . f,
       _categoryOfMove = _categoryOfMove w . f,
       _typeOfPokemon = \cp -> _typeOfPokemon w cp . f}

reduce :: WeatherDamage battle -> battle -> Damage
reduce w b =
  let
    mtp = view typeOfMove w b
    cw = view currentWeather w b
    cm = view categoryOfMove w b
    trgt = view typeOfPokemon w Target b
    eff = view effectOfType w cw mtp
    ss =  view effectOfSandstorm w cw trgt cm
    in Damage eff <> Damage ss
