{-# LANGUAGE ViewPatterns, RecordWildCards #-}

module Domain.SideEffect.Poisoned where

import Domain.SideEffect.AilmentCommon
import Domain.Algebra.Path
import Domain.Attribute.TypeOf
import Domain.Attribute.Ability
import Domain.Pokemon.Species
import Control.Applicative

type PoisonBlocked = AilmentBlockingExtended IsImmunity IsPoisonType

type TargetPoisoned =
  Either (MoldBreakerBlocks PoisonBlocked)
  (Path PoisonBlocked (TargetHasSynchronize UserPoisoned))

type UserPoisoned =
  Path PoisonBlocked ()

data IsImmunity = IsImmunity deriving (Eq,Show)

data IsPoisonType = IsPoisonType | IsSteelType deriving (Eq,Show)

isImmunity :: Ability -> Maybe IsImmunity
isImmunity Immunity = Just IsImmunity
isImmunity _ = Nothing

isPoisonType :: TypeOfPokemon -> Maybe IsPoisonType
isPoisonType (elem Poison . getTypeOfPokemon -> True) = Just IsPoisonType
isPoisonType (elem Steel . getTypeOfPokemon -> True) = Just IsSteelType
isPoisonType _ = Nothing

isImmunity' :: Ability -> Maybe (AilmentBlocking IsImmunity)
isImmunity' ab = do
  a <- isImmunity ab
  return $ HasAilmentBlockingAbility a

ailmentBlocking' ::
  AilmentData battle
  -> battle
  -> Maybe (AilmentBlocking IsImmunity)
ailmentBlocking' ad@AilmentData{..} b =
  ailmentBlocking ad b <|>
  isImmunity' (pokemonHasAbility counterParty b)

ailmentBlockingExtended ::
  AilmentData battle
  -> battle
  -> Maybe (AilmentBlockingExtended IsImmunity IsPoisonType)
ailmentBlockingExtended ad@AilmentData{..} b =
  (BlockingByType <$> isPoisonType (pokemonHasType counterParty b)) <|>
  (BlockingByAbility <$> ailmentBlocking' ad b)
