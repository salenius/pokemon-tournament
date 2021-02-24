module Domain.Attribute.MoveExecution where

import Domain.Attribute.Damage

data MoveExecution = MoveExecution
  {
    damageCaused :: Damage
  , recoilCaused :: Damage
  , hitWasCritical :: Bool
  } deriving (Eq,Show)


data PreviousStrike =
  FailedToExecute
  | ChargingMove
  | FailedToHit
  | CausesDamage MoveExecution
  deriving (Eq,Show)
