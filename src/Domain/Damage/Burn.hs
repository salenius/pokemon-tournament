module Domain.Damage.Burn where

import Attribute 
import Domain.Attribute
import Domain.Damage.Common
import Types.Pokemon

class (MonadLogic m,
      GetCounterparty m,
      IgnoreAbility m,
      MoveAttribute m,
      PokemonAttribute m) => BurnDamageCalc m where
  abilityNonPreventing :: m PokemonAbility
  boostForCategoryOf :: Ailment -> Counterparty -> m (MoveCategory, PokemonAbility, Double)
  boostForTypeOf :: Ailment -> m (TypeOf, PokemonAbility, Double)

ailmentBoost :: BurnDamageCalc m => m Double
ailmentBoost = do
  gts <- multiplying gutsBoost
  msc <- multiplying marvelScaleBoost
  return $ gts * msc

generalBoost :: BurnDamageCalc m => Counterparty -> m Double
generalBoost cp = do
  usr <- counterparty cp
  ab  <- pokemonAbility usr
  ail <- pokemonAilment usr
  nab <- abilityNonPreventing
  mvc <- moveCategory
  (cat, ab', dbl) <- boostForCategoryOf ail cp
  cat === mvc
  ab' === ab
  ab === nab
  return dbl

gutsBoost :: BurnDamageCalc m => m Double
gutsBoost = generalBoost User

marvelScaleBoost :: BurnDamageCalc m => m Double
marvelScaleBoost = generalBoost Target
