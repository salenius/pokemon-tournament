{-# LANGUAGE ViewPatterns, RecordWildCards, MultiParamTypeClasses, FunctionalDependencies, TemplateHaskell, FlexibleInstances #-}

module Domain.SideEffect.AilmentCommon (
  AilmentData(..),
  AilmentReader,
  AilmentToEffect,
  toEffect,
  UserAilmented(),
  TargetAilmented(),
  AilmentBlocking(),
  AbilityBlocks(),
  TypeBlocking(),
  MoldBreakerCancels(),
  abilityBlocks,
  hasLeafGuard,
  hasSafeguard,
  hasAlreadyAilment,
  hasBlockingType,
  userAilmented,
  moldBreakerBlocks,
  hasBlockingAbility,
  targetAilmented
  
                                       ) where

import Data.Functor.Contravariant
import Domain.Attribute.Counterparty
import Domain.Attribute.Ailment
import Domain.Attribute.Ability
import Domain.Attribute.Screen
import Domain.Pokemon.Species
import Domain.Attribute.Weather
import Domain.Algebra.Effect
import Control.Monad.Reader
import Control.Lens
import Data.Maybe
import Control.Applicative


data AilmentData battle = AilmentData
  {
    counterparty :: Counterparty
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

type AilmentReader battle a = ReaderT (AilmentData battle, battle) Maybe a

data IsComatose = IsComatose deriving (Eq,Show)

data AbilityBlocks ability =
  HasLeafGuard
  | AbilityBlocks ability
  | HasComatose IsComatose
  deriving (Eq,Show)


makeClassyPrisms ''AbilityBlocks

data IsAilmentedBy =
  IsAlreadyBurned
  | IsAlreadyParalyzed
  | IsAlreadyFrozen
  | IsAlreadyPoisoned
  | IsAlreadySleep
  deriving (Eq,Show)

data AilmentBlocking ability =
  HasBlockingAbility (AbilityBlocks ability)
  | HasSafeguard
  | HasAlreadyAilment IsAilmentedBy
  deriving (Eq,Show)


makeClassyPrisms ''AilmentBlocking

data MoldBreakerCancels ability =
  MoldBreakerCancels IsMoldBreaker (AbilityBlocks ability)
  deriving (Eq,Show)


data TypeBlocking tyyppi = HasBlockingType tyyppi
  deriving (Eq,Show)


data SpawnNewEffect b = TargetHasSynchronize b
  deriving (Eq,Show)

data UserAilmented ability tyyppi =
  UserAilmentBlocked (AilmentBlocking ability)
  | UserAilmentBlockedByType (TypeBlocking tyyppi)
  | UserAilmentGeneralCase
  deriving (Eq,Show)


data TargetAilmented ability tyyppi =
  TargetAilmentBlocked (AilmentBlocking ability)
  | TargetAilmentBlockedByType (TypeBlocking tyyppi)
  | MoldBreakerCanceling (MoldBreakerCancels ability)
  | NewEffectSpawned (SpawnNewEffect (UserAilmented ability tyyppi))
  | TargetAilmentGeneralCase
  deriving (Eq,Show)

makeClassyPrisms ''UserAilmented
makeClassyPrisms ''TargetAilmented


class AilmentToEffect ailm where
  toEffect :: ailm -> Effect ailm

instance AilmentToEffect (UserAilmented ability tyyppi) where
  toEffect = userAilmentedToEffect

instance AilmentToEffect (TargetAilmented ability tyyppi) where
  toEffect = targetAilmentedToEffect

-- Parsers

userAilmented
  :: (Ability -> Maybe ab)
  -> (TypeOfPokemon -> Maybe tp)
  -> AilmentData battle
  -> battle
  -> UserAilmented ab tp
userAilmented abf tpf ad b =
  fromMaybe UserAilmentGeneralCase .
  flip runReaderT (ad,b) $
  UserAilmentBlockedByType <$> hasBlockingType tpf <|>
  UserAilmentBlocked <$> (hasAlreadyAilment <|>
                          hasSafeguard <|>
                          hasBlockingAbility abf)

targetAilmented
  :: (Ability -> Maybe ab)
  -> (TypeOfPokemon -> Maybe tp)
  -> AilmentData battle
  -> battle
  -> TargetAilmented ab tp
targetAilmented abf tpf ad b =
  fromMaybe TargetAilmentGeneralCase .
  flip runReaderT (ad,b) $
  TargetAilmentBlockedByType <$> hasBlockingType tpf <|>
  MoldBreakerCanceling <$> moldBreakerBlocks abf <|>
  TargetAilmentBlocked <$> hasAlreadyAilment <|>
  TargetAilmentBlocked <$> hasSafeguard <|>
  TargetAilmentBlocked <$> hasBlockingAbility abf <|>
  NewEffectSpawned <$> targetHasSynchronize abf tpf



abilityBlocks :: (Ability -> Maybe a) -> AilmentReader battle (AbilityBlocks a)
abilityBlocks prser =
  abilityBlocks' prser <|>
  hasLeafGuard <|>
  hasComatose

hasLeafGuard :: AilmentReader battle (AbilityBlocks a)
hasLeafGuard = do
  (AilmentData{..}, battle) <- ask
  let weather = currentWeatherIs battle
  let ability = pokemonHasAbility counterparty battle
  lift $ case (weather,ability) of
    (Sunny, LeafGuard) -> Just HasLeafGuard
    _ -> Nothing

isComatose :: Ability -> Maybe IsComatose
isComatose Comatose = Just IsComatose
isComatose _ = Nothing

hasComatose :: AilmentReader battle (AbilityBlocks a)
hasComatose = do
  (AilmentData{..}, battle) <- ask
  let ability = pokemonHasAbility counterparty battle
  lift $ HasComatose <$> isComatose ability

abilityBlocks' :: (Ability -> Maybe a) -> AilmentReader battle (AbilityBlocks a)
abilityBlocks' prser = do
  (AilmentData{..}, battle) <- ask
  let ability = pokemonHasAbility counterparty battle
  lift $ AbilityBlocks <$> prser ability


hasSafeguard' :: Safeguard -> Maybe (AilmentBlocking ability)
hasSafeguard' NoSafeguard = Nothing
hasSafeguard' (Safeguard _) = Just HasSafeguard

hasAlreadyAilment' :: Ailment -> Maybe (AilmentBlocking ability)
hasAlreadyAilment' Healthy = Nothing
hasAlreadyAilment' x = Just . HasAlreadyAilment $ case x of
  Burned -> IsAlreadyBurned
  Paralyzed -> IsAlreadyParalyzed
  Poisoned -> IsAlreadyPoisoned
  Frozen -> IsAlreadyFrozen
  Sleep -> IsAlreadySleep

hasSafeguard :: AilmentReader battle (AilmentBlocking ability)
hasSafeguard = do
  (AilmentData{..}, battle) <- ask
  let sg = pokemonHasSafeguard counterparty battle
  lift $ hasSafeguard' sg

hasAlreadyAilment :: AilmentReader battle (AilmentBlocking ability)
hasAlreadyAilment = do
  (AilmentData{..}, battle) <- ask
  let ailm = pokemonHasAilment counterparty battle
  lift $ hasAlreadyAilment' ailm

hasBlockingAbility :: (Ability -> Maybe a) -> AilmentReader battle (AilmentBlocking a)
hasBlockingAbility prser = HasBlockingAbility <$> abilityBlocks prser

moldBreakerBlocks ::
  (Ability -> Maybe ability) -> AilmentReader battle (MoldBreakerCancels ability)
moldBreakerBlocks prser = do
  (AilmentData{..}, battle) <- ask
  ab <- lift $ isMoldBreaker . pokemonHasAbility User $ battle
  ab' <- abilityBlocks prser
  return $ MoldBreakerCancels ab ab'

hasBlockingType ::
  (TypeOfPokemon -> Maybe tyyppi) -> AilmentReader battle (TypeBlocking tyyppi)
hasBlockingType prser = do
  (AilmentData{..}, battle) <- ask
  tp <- lift $ prser $ pokemonHasType counterparty battle
  return $ HasBlockingType tp

targetHasSynchronize ::
  (Ability -> Maybe ab)
  -> (TypeOfPokemon -> Maybe tp)
  -> AilmentReader battle (SpawnNewEffect (UserAilmented ab tp))
targetHasSynchronize abf tpf = do
  (ad@AilmentData{..}, battle) <- ask
  let ab = pokemonHasAbility Target battle
  case ab of
    Synchronize -> do
      let ad' = ad {counterparty = User}
      let eff = userAilmented abf tpf ad' battle
      return $ TargetHasSynchronize eff
    _ -> lift Nothing





-- Apufunktiot



targetAilmentedToEffect ::
  TargetAilmented ability tyyppi -> Effect (TargetAilmented ability tyyppi)
targetAilmentedToEffect (preview _MoldBreakerCanceling -> Just a) =
  return $ review _MoldBreakerCanceling a
targetAilmentedToEffect (preview _TargetAilmentBlocked -> Just a) =
  return $ review _TargetAilmentBlocked a
targetAilmentedToEffect (preview _NewEffectSpawned -> Just a) =
  return $ review _NewEffectSpawned a
targetAilmentedToEffect (preview _TargetAilmentBlockedByType -> Just a) =
  return $ review _TargetAilmentBlockedByType a
targetAilmentedToEffect _ = return $ TargetAilmentGeneralCase

userAilmentedToEffect ::
  UserAilmented ability tyyppi -> Effect (UserAilmented ability tyyppi)
userAilmentedToEffect (preview _UserAilmentBlocked -> Just a) =
  return $ review _UserAilmentBlocked a
userAilmentedToEffect (preview _UserAilmentBlockedByType -> Just a) =
  return $ review _UserAilmentBlockedByType a
userAilmentedToEffect _ = return $ UserAilmentGeneralCase
