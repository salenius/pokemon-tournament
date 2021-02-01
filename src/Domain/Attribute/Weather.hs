module Domain.Attribute.Weather where

data Weather =
  Normal
  | Sunny
  | Rainy
  | Hail
  | Sandstorm
  deriving (Show,Eq)

allWeathers :: [Weather]
allWeathers =
  [
  Normal
  , Sunny
  , Rainy
  , Hail
  , Sandstorm
  ]
