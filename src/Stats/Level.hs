module Stats.Level (
  Level(),
  levelToInt,
  intToLevel,
  defaultLevel,
  fullLevel
                   ) where

newtype Level = Level {levelToInt :: Int} deriving (Eq,Show,Read,Ord)

intToLevel :: Int -> Maybe Level
intToLevel i
  | i < 1 = Nothing
  | i > 100 = Nothing
  | otherwise = Just $ Level i

defaultLevel :: Level
defaultLevel = Level 50

fullLevel :: Level
fullLevel = Level 100
