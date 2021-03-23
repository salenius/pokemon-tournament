{-# LANGUAGE TemplateHaskell #-}

module Domain.Attribute.MoveExecution where

import Domain.Attribute.Damage
import Control.Lens

data MoveExecution = MoveExecution
  {
    _damageCaused :: Damage
  , _recoilCaused :: Damage
  , _hitWasCritical :: Bool
  } deriving (Eq,Show)


data PreviousStrike =
  FailedToExecute
  | ChargingMove
  | FailedToHit
  | CausesDamage MoveExecution
  deriving (Eq,Show)

makeLenses ''MoveExecution
makePrisms ''PreviousStrike
