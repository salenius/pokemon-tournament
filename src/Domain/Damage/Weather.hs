module Domain.Damage.Weather where

import Attribute
import Types.Pokemon
import Domain.Attribute

class (MonadLogic m,
      Alternative m,
      PokemonAttribute m,
      GetCounterparty m) => WeatherDamageCalc m where
  -- Fire vs Water in rain or sunny
  weatherBoostForType :: m (TypeOf, Weather, Double)
  -- Psychic moves boosted in Psychic Terrain etc
  terrainBoostForType :: m (TypeOf, Terrain, Double)
  -- Rock type's SpDefence boost in sandstorm
  defenceBoostInWeather :: m (PokemonType, Weather, MoveCategory, Double)
  moveType :: m TypeOf
  moveCategory :: m MoveCategory
  weather :: m Weather
  terrain :: m Terrain


weatherDamageCalc :: WeatherDamageCalc m => m Double
weatherDamageCalc = do
  (tp1, w1, d1) <- weatherBoostForType
  (tp2, tr1, d2) <- terrainBoostForType
  (pt, w2, c', d3) <- defenceBoostInWeather
  trgt <- counterparty Target
  trgtt <- pokemonType trgt
  mvt <- moveType
  w <- weather
  tr <- terrain
  cat <- moveCategory
  tp1 === mvt
  tp2 === mvt
  w === w1
  w === w2
  tr === tr1
  pt === trgtt
  cat === c'
  return $ d1 * d2 * d3
  
