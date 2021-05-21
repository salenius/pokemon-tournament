
module Domain.Damage.CriticalHit where

-- import Attribute.Counterparty
-- import Attribute.Ability
-- import Attribute.Item
-- import Attribute.Ailment
import Attribute
import Domain.Logic
import Prelude hiding (not,and,or,when)
import Domain.Common

class (CounterpartyGetter m,
       PokemonAttrGetter m,
       MoldBreakerGetter m,
       Monad m) => CritHit m where
  scopeLens :: Player -> m Bool
  scopeLens = heldItemIs _ScopeLens
  merciless :: Player -> m Bool 
  merciless = abilityIs _Merciless
  battleArmor :: Player -> m Bool 
  battleArmor = abilityIs _BattleArmor
  shellArmor :: Player -> m Bool
  shellArmor = abilityIs _ShellArmor
  increaseLevel :: m Action
  nullifyLevel :: m Action
  levelToTop :: m Action
  getCriticalHit :: m CriticalHit


program :: CritHit m => m CriticalHit
program = do
  userHas scopeLens
    --> increaseLevel
  userHas merciless `and` targetHas poisoned
    --> levelToTop 
  targetHas shellArmor `or` targetHas battleArmor
    `and'`
    userHas ~/ moldBreaker'
    --> nullifyLevel
  getCriticalHit
