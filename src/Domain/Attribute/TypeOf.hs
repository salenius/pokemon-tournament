module Domain.Attribute.TypeOf where

data TypeOf =
  Normal
  | Flying
  | Fighting
  | Fire
  | Water
  | Grass
  | Electric
  | Rock
  | Ground
  | Poison
  | Bug
  | Steel
  | Psychic
  | Ice
  | Dark
  | Ghost
  | Dragon
  | Fairy
  deriving (Eq, Show)

allTypes :: [TypeOf]
allTypes =
  [
   Normal
  , Flying
  , Fighting
  , Fire
  , Water
  , Grass
  , Electric
  , Rock
  , Ground
  , Poison
  , Bug
  , Steel
  , Psychic
  , Ice
  , Dark
  , Ghost
  , Dragon
  , Fairy
 ] 

allTypeCombinations :: [[TypeOf]]
allTypeCombinations = (do
  x <- allTypes
  y <- allTypes
  return $ [x,y]) ++ map (\x -> [x]) allTypes
