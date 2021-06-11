module Attribute.MoveExecution where

import Attribute.Damage
import Types.Effect
import Attribute.CriticalHit

data MoveExecution = MoveExecution
  {
    _damageDealt :: Maybe Damage
  , _typeAdvantage :: Maybe TypeEffect
  , _possibleCrit :: Maybe CriticalHit
  }
