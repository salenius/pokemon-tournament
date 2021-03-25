{-# LANGUAGE RecordWildCards #-}

module Domain.SideEffect.Flinch (
  FlinchCondition(),
  mkFlinchEffect,
  FlinchData(..),
  mapFlinch
                                ) where

import Domain.Attribute.Ability
import Domain.Attribute.Counterparty
import Domain.Algebra.Effect
import Data.Maybe
import Control.Applicative

data FlinchCondition sideEff =
  GeneralCondition
  | TargetHasSteadfast sideEff
  | TargetHasInnerFocus
  | UserHasMoldBreaker IsMoldBreaker (FlinchCondition sideEff)
  deriving (Eq,Show)

instance Functor FlinchCondition where
  fmap f (TargetHasSteadfast x) = TargetHasSteadfast $ f x
  fmap f (UserHasMoldBreaker m x) = UserHasMoldBreaker m $ fmap f x
  fmap _ GeneralCondition = GeneralCondition
  fmap _ TargetHasInnerFocus = TargetHasInnerFocus

data FlinchData battle = FlinchData
  {
    abilityOfPokemon :: Counterparty -> battle -> Ability
  }

mkFlinchEffect :: FlinchData battle -> battle -> Effect (FlinchCondition ())
mkFlinchEffect d = toEffect . parseFlinch d

mapFlinch :: (a -> b) -> Effect (FlinchCondition a) -> Effect (FlinchCondition b)
mapFlinch f = fmap (fmap f)

parseFlinch :: FlinchData battle -> battle -> FlinchCondition ()
parseFlinch FlinchData{..} battle =
  fromMaybe GeneralCondition $
  parseSteadfast abilityOfPokemon battle <|>
  parseMoldBreaker abilityOfPokemon battle <|>
  parseInnerFocus abilityOfPokemon battle 

toEffect :: FlinchCondition a -> Effect (FlinchCondition a)
toEffect TargetHasInnerFocus = doNothing
toEffect x = return x

------

parseSteadfast :: (Counterparty -> b -> Ability) -> b -> Maybe (FlinchCondition ())
parseSteadfast fn b =
  case fn Target b of
    Steadfast -> Just (TargetHasSteadfast ())
    _ -> Nothing


parseInnerFocus :: (Counterparty -> b -> Ability) -> b -> Maybe (FlinchCondition ())
parseInnerFocus fn b =
  case fn Target b of
    InnerFocus -> Just TargetHasInnerFocus
    _ -> Nothing


parseMoldBreaker :: (Counterparty -> b -> Ability) -> b -> Maybe (FlinchCondition ())
parseMoldBreaker fn b = do
  inf' <- parseInnerFocus fn b
  mbr' <- isMoldBreaker $ fn User b
  return $ UserHasMoldBreaker mbr' inf'
