{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}


module Domain.Common where

import Attribute.MoveExecution
import Attribute.Counterparty
import Attribute.Ability
import Attribute.HP
import Attribute.Weather
import Attribute.Item
import Domain.Logic
import Prelude hiding (or)
import Control.Lens
import Data.Maybe
import Types.Pokemon

class AbilityGetter m => MoldBreakerGetter m where
  moldBreaker :: Player -> m Bool
  teravolt :: Player -> m Bool
  turboblaze :: Player -> m Bool

class Monad m => AilmentGetter m where
  poisoned :: Player -> m Bool
  burned :: Player -> m Bool

class Monad m => StatusGetter m where
  isFlinched :: Player -> m Bool
  isConfused :: Player -> m Bool
  isProtected :: Player -> m Bool
  isInfatuated :: Player -> m Bool
  isHealBlocked :: Player -> m Bool
  isCursed :: Player -> m Bool
  isTormented :: Player -> m Bool
  isTaunted :: Player -> m Bool
  isLeechSeeded :: Player -> m Bool
  isYawned :: Player -> m Bool

class Monad m => CounterpartyGetter m where
  getUser :: m Player
  getTarget :: m Player

class Monad m => AbilityGetter m where
  getAbility :: Player -> m PokemonAbility
  abilityIs :: Prism' PokemonAbility a -> (Player -> m Bool)
  abilityIs pr pl = is pr <$> getAbility pl

class Monad m => HeldItemGetter m where
  getHeldItem :: Player -> m (Maybe HeldItem)
  heldItemIs :: Prism' HeldItem a -> (Player -> m Bool)
  heldItemIs pr pl = do
    it <- getHeldItem pl
    return $ fromMaybe False $ do
      it' <- it
      return $ is pr it'

class (AbilityGetter m,
      HeldItemGetter m,
      StatusGetter m,
      AilmentGetter m) => PokemonAttrGetter m where
  getHp :: Player -> m HP
  getTypeOf :: Player -> m PokemonType
  getWeight :: Player -> m Double
  getLevel :: Player -> m Int

moldBreaker' :: MoldBreakerGetter m => Player -> m Bool
moldBreaker' cp =
  moldBreaker cp `or`
  teravolt cp `or`
  turboblaze cp

userHas :: CounterpartyGetter m => (Player -> m Bool) -> m Bool
userHas f = do
  usr <- getUser
  b <- f usr
  return b

targetHas :: CounterpartyGetter m => (Player -> m Bool) -> m Bool
targetHas f = do
  trgt <- getTarget
  b <- f trgt
  return b

userDoes :: (Monoid a, CounterpartyGetter m) => (Player -> m a) -> m a
userDoes f = do
  usr <- getUser
  f usr

targetDoes :: (Monoid a, CounterpartyGetter m) => (Player -> m a) -> m a
targetDoes f = do
  trgt <- getTarget
  f trgt

class (CounterpartyGetter m, PokemonAttrGetter m) => BattleStrikeGetter m where
  getWeather :: m Weather
  getPrevStrike :: m (Maybe MoveExecution)
