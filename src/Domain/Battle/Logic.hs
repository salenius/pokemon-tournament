module Domain.Battle.Logic where

import Domain.Attribute.Counterparty
import Domain.Attribute.Ailment
import Domain.Attribute.MoveExecution
import Domain.Battle.Algebra
import Domain.Match.Validation
import Control.Monad.Reader
import Control.Lens (preview,view)
import Domain.Entity.PokemonState as PS
import Domain.Entity.Battle

class BattleLogic l where
  evalLogic :: (BattleAlgebra b, ReadPreviousStrike b) => b -> l -> Bool

instance BattleLogic l => BattleLogic (Logic l) where
  evalLogic b (Statement a) = evalLogic b a
  evalLogic b (Not a) = not $ evalLogic b a
  evalLogic b' (And a b) = (&&) (evalLogic b' a) $ evalLogic b' b
  evalLogic b' (Or a b) = (||) (evalLogic b' a) $ evalLogic b' b
  evalLogic b' AlwaysTrue = True

asPokemonGetter User = getUserPokemon
asPokemonGetter Target = getTargetPokemon

counterpartyLogic l cp fn b = attributeSatisfies ((asPokemonGetter cp) (view l)) fn b

instance BattleLogic BattleValidation where
  evalLogic b (AbilityIs cp fn) =
    counterpartyLogic (PS.currentAttributes . PS.ability) cp fn b
  evalLogic b (HeldItemIs cp fn) =
    counterpartyLogic (PS.currentAttributes . PS.heldItem) cp fn b
  evalLogic b (TypeIs cp fn) =
    counterpartyLogic (PS.currentAttributes . PS.typeOf) cp fn b
  evalLogic b (AilmentIs cp fn) =
    counterpartyLogic (PS.currentStatus . PS.ailment) cp (fn . fn') b
    where
      fn' IsHealthy = Healthy
      fn' IsBurned = Burned
      fn' IsParalyzed = Paralyzed
      fn' IsPoisoned = Poisoned
      fn' (IsSleep _) = Sleep
      fn' (IsFrozen _) = Frozen
  evalLogic b (WeatherIs fn) =
    attributeSatisfies getWeather fn b
  evalLogic b (MoveExecutionIs fn) =
    attributeSatisfies askPrevStrike fn b
  evalLogic _ _ = True
