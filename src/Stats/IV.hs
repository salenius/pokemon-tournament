module Stats.IV (
  IV(),
  ivToInt,
  intToIv,
  maxIv
                ) where

newtype IV = IndividualValue {ivToInt :: Int} deriving (Eq,Show,Ord)

intToIv :: Int -> Maybe IV
intToIv i
  | i < 0 = Nothing
  | i > 31 = Nothing
  | otherwise = Just $ IndividualValue i

maxIv = IndividualValue 31
