module Domain.Attribute.Choice where

data ChoiceProbability v =
  NormalProbability
  | UseOnce
  | LockableMove
  | OnlyPossibilityIf v
  deriving (Eq,Show)

data ChoiceRule v =
  ChoiceRule (ChoiceProbability v)
  | Locked LockingType (ChoiceRule v)
  | UnableToUse LockingType (ChoiceRule v)
  deriving (Eq,Show)

data LockingType =
  LockedForTurns Int
  | LockedPermanently
  deriving (Eq,Show)
