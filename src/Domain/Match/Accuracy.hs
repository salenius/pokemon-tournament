module Domain.Match.Accuracy where

import Domain.Match.Validation
import Domain.Attribute.Weather

data Accuracy =
  Accuracy Double
  | AlwaysHits
  deriving (Eq,Show,Ord)

data ProbabilityChangeFactor =
  LevelGap
  | SpeedGap
  deriving (Eq,Show)

data HitProbability =
  BasicAccuracy Accuracy
  | HitsAlwaysIf Validation HitProbability
  | ChangesByFactor ProbabilityChangeFactor HitProbability
  deriving (Eq,Show)

alwaysHitsInWeather :: Weather -> HitProbability -> HitProbability
alwaysHitsInWeather w prob =
  let cond = weatherIs (\w' -> w' == w)
  in HitsAlwaysIf cond prob


