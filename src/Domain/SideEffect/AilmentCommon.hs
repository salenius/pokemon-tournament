{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Domain.SideEffect.AilmentCommon where

import Data.Functor.Contravariant
import Domain.Attribute.Counterparty
import Domain.Attribute.Ailment
import Domain.Attribute.Ability
import Domain.Attribute.Screen
import Domain.Pokemon.Species
import Domain.Attribute.Weather
import Control.Applicative


data AilmentBlocking ability =
  HasLeafGuard
  | HasAilmentBlockingAbility ability
  | HasComatose IsComatose
  | HasSafeguard
  | HasAlreadyAilment IsAilmentedBy
  deriving (Eq,Show)

data MoldBreakerBlocks ability =
  MoldBreakerBlocks IsMoldBreaker (AilmentBlocking ability)
  deriving (Eq,Show)

data AilmentBlockingExtended ability tyyppi =
  BlockingByAbility (AilmentBlocking ability)
  | BlockingByType tyyppi
  deriving (Eq,Show)

data IsComatose = IsComatose deriving (Eq,Show)

data IsAilmentedBy =
  IsAlreadyBurned
  | IsAlreadyParalyzed
  | IsAlreadyFrozen
  | IsAlreadyPoisoned
  | IsAlreadySleep
  deriving (Eq,Show)

newtype TargetHasSynchronize a = TargetHasSynchronize a


data AilmentData battle = AilmentData
  {
    counterParty :: Counterparty
  , ailmentInQuestion :: Ailment
  , pokemonHasAbility :: Counterparty -> battle -> Ability
  , pokemonHasAilment :: Counterparty -> battle -> Ailment
  , currentWeatherIs :: battle -> Weather
  , pokemonHasType :: Counterparty -> battle -> TypeOfPokemon
  , pokemonHasSafeguard :: Counterparty -> battle -> Safeguard
  }

instance Contravariant AilmentData where
  contramap f x = x {pokemonHasAbility = \c -> pokemonHasAbility x c . f
                    ,pokemonHasAilment = \c -> pokemonHasAilment x c . f
                    ,pokemonHasType = \c -> pokemonHasType x c . f
                    ,pokemonHasSafeguard = \c -> pokemonHasSafeguard x c . f
                    ,currentWeatherIs = currentWeatherIs x . f
                    }

ailmentBlocking
  :: AilmentData battle
  -> battle
  -> Maybe (AilmentBlocking a)
ailmentBlocking AilmentData{..} battle =
  hasLeafGuard (currentWeatherIs battle) (pokemonHasAbility counterParty battle) <|>
  hasComatose (pokemonHasAbility counterParty battle) <|>
  hasSafeguard (pokemonHasSafeguard counterParty battle) <|>
  hasAlreadyAilment (pokemonHasAilment counterParty battle)


hasLeafGuard :: Weather -> Ability -> Maybe (AilmentBlocking ability)
hasLeafGuard Sunny LeafGuard = Just HasLeafGuard
hasLeafGuard _ _ = Nothing

hasComatose :: Ability -> Maybe (AilmentBlocking ability)
hasComatose ab = HasComatose <$> isComatose ab

hasSafeguard :: Safeguard -> Maybe (AilmentBlocking ability)
hasSafeguard NoSafeguard = Nothing
hasSafeguard (Safeguard _) = Just HasSafeguard

hasAlreadyAilment :: Ailment -> Maybe (AilmentBlocking ability)
hasAlreadyAilment Healthy = Nothing
hasAlreadyAilment x = Just . HasAlreadyAilment $ case x of
  Burned -> IsAlreadyBurned
  Paralyzed -> IsAlreadyParalyzed
  Poisoned -> IsAlreadyPoisoned
  Frozen -> IsAlreadyFrozen
  Sleep -> IsAlreadySleep

isComatose :: Ability -> Maybe IsComatose
isComatose Comatose = Just IsComatose
isComatose _ = Nothing
