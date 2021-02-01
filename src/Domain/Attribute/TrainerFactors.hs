module Domain.Attribute.TrainerFactors where

data TrainerClass =
  Amateur
  | Leader
  | Champion
  deriving (Eq,Show,Read,Ord)

data Region =
  Kanto
  | Johto
  | Hoenn
  | Sinnoh
  | Unova
  | RealWorld Country
  deriving (Eq,Show,Read)

newtype Country = Country String
  deriving (Eq,Show,Read)
