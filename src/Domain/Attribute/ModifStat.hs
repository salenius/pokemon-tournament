module Domain.Attribute.ModifStat where

data ModifStat =
  Attack'
  | Defence'
  | SAttack'
  | SDefence'
  | Speed'
  | Accuracy'
  | Evasion'
  deriving (Eq,Show,Ord)
