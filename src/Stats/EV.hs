module Stats.EV (
  EV(),
  evToInt,
  intToEv,
  maxEv
                ) where

newtype EV = EffortValue {evToInt :: Int} deriving (Eq,Show,Ord)

intToEv :: Int -> Maybe EV
intToEv i
  | i < 0 = Nothing
  | i > 255 = Nothing
  | otherwise = Just $ EffortValue i

maxEv = EffortValue 255
