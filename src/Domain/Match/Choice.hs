module Domain.Match.Choice where

data ChoiceProbability =
  NormalProbability
  | UseOnce
  deriving (Eq,Show)

data ChoiceRule =
  ChoiceRule ChoiceProbability
  | Locked LockingType ChoiceRule
  | UnableToUse LockingType ChoiceRule
  deriving (Eq,Show)

data LockingType =
  LockedForTurns Int
  | LockedPermanently
  deriving (Eq,Show)
