{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Domain.SideEffect.LeechSeed (
  LeechSeedData(..),
  mkLeechSeed
                                   ) where

import Domain.Pokemon.Species
import Domain.Attribute.Counterparty
import Data.Maybe
import Data.Functor.Contravariant
import Domain.Attribute.TypeOf
import Domain.Algebra.Effect

data LeechSeedData battle = LeechSeedData
  {
    typeOfPokemon :: Counterparty -> battle -> TypeOfPokemon
  }

instance Contravariant LeechSeedData where
  contramap f x = x {typeOfPokemon = \cp -> typeOfPokemon x cp . f}

data LeechSeedCondition =
  GeneralCondition
  | TargetHasGrassType
  deriving (Eq,Show)

mkLeechSeed :: LeechSeedData battle -> battle -> Effect LeechSeedCondition
mkLeechSeed d = toEffect . parseLeechSeed d

parseLeechSeed :: LeechSeedData battle -> battle -> LeechSeedCondition
parseLeechSeed LeechSeedData{..} battle =
  fromMaybe GeneralCondition $ parseGrassType typeOfPokemon battle

parseGrassType
  :: (Counterparty -> battle -> TypeOfPokemon) -> battle -> Maybe LeechSeedCondition
parseGrassType f b =
  case f Target b of
    (elem Grass . getTypeOfPokemon -> True) -> Just TargetHasGrassType
    _ -> Nothing

toEffect :: LeechSeedCondition -> Effect LeechSeedCondition
toEffect GeneralCondition = return GeneralCondition
toEffect TargetHasGrassType = doNothing
