module Domain.Damage.Info where

import Domain.Attribute.Damage
import Domain.Attribute.CriticalHit
import Domain.Attribute.TypeOf

data DamageInfo = DamageInfo
  {
    getDamage :: Damage
  , getCriticalHit :: CriticalHit
  , getTypeEffectiveness :: TypeEffect
  } deriving (Eq,Show)
