{-# LANGUAGE ViewPatterns #-}


module Domain.Damage.TypeAdvantage where

import Attribute
import Domain.Attribute
import Types.BuiltIn
import Types.Pokemon
import Types.Effect

class (MonadLogic m,
      GetCounterparty m,
      Alternative m,
      PokemonAttribute m,
      IgnoreItem m,
      IgnoreAbility m) => TypeAdvantageCalc m where
  typeBerry :: m (HeldItem, TypeOf, TypeEffect)
  supereffectiveItem :: m (HeldItem, Double)
  supereffectiveReducingAbility :: m PokemonAbility
  typeAdvantageCalc :: m (TypeOf -> TypeOf -> TypeEffect)
  moveTypeOf :: m TypeOf
  dropItem :: Player -> m ()


typeAdvantage :: TypeAdvantageCalc m => m Double
typeAdvantage = do
  tp <- typeAdvantageCalc
  trgt <- counterparty Target
  ptyp <- pokemonType trgt
  mvtp <- moveTypeOf
  let mult = calcTypeEffect tp mvtp ptyp
  eb <- once $ expertBelting mult <|> return mult
  br <- once $ berrying eb <|> return eb
  sr <- once $ solidRocking br <|> return br
  return $ sr
  
calcTypeEffect :: (TypeOf -> TypeOf -> TypeEffect) -> TypeOf -> PokemonType -> Double
calcTypeEffect tp mvt = foldr (*) 1 . fmap effectAsDouble . fmap (tp mvt)

doubleToEffect mult = case mult of
                        ((==) 0 -> True) -> NoEffect
                        ((<) 1 -> True) -> NotVeryEffective
                        ((>) 1 -> True) -> SuperEffective
                        _ -> NormalEffect

expertBelting :: TypeAdvantageCalc m => Double -> m Double
expertBelting dbl = do
  usr <- counterparty User
  it  <- pokemonHeldItem usr
  (sei, mult) <- supereffectiveItem
  sei === it <&&> ableToUseItem usr
  guard $ doubleToEffect dbl == SuperEffective
  return $ mult * dbl

berrying :: TypeAdvantageCalc m => Double -> m Double
berrying dbl = do
  trgt <- counterparty Target
  it   <- pokemonHeldItem trgt
  mvt  <- moveTypeOf
  (br, tp, eff) <- typeBerry
  let eff' = doubleToEffect dbl
  eff' === eff <&&> it === br <&&> mvt === tp <&&> ableToUseItem trgt
  dropItem trgt
  return $ dbl * 0.5

solidRocking :: TypeAdvantageCalc m => Double -> m Double
solidRocking dbl = do
  trgt <- counterparty Target
  ab   <- pokemonAbility trgt
  let eff = doubleToEffect dbl
  sab  <- supereffectiveReducingAbility
  ab === sab <|> ignoreTargetAbility
  eff === SuperEffective
  return $ dbl * 0.5
