module Domain.Attribute.Weather where

data Weather =
  Normal
  | Sunny
  | Rainy
  | Hail
  | Sandstorm
  deriving (Show,Eq)

data WeatherStatus =
  NormalWeather
  | SunnyWeather Rounds
  | RainyWeather Rounds
  | HailWeather Rounds
  | SandstormWeather Rounds
  deriving (Eq,Show)

type Rounds = Int

weatherStatusToWeather :: WeatherStatus -> Weather
weatherStatusToWeather ws = case ws of
  NormalWeather -> Normal
  SunnyWeather _ -> Sunny
  RainyWeather _ -> Rainy
  HailWeather _ -> Hail
  SandstormWeather _ -> Sandstorm

allWeathers :: [Weather]
allWeathers =
  [
  Normal
  , Sunny
  , Rainy
  , Hail
  , Sandstorm
  ]
