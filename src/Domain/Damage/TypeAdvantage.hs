{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Domain.Damage.TypeAdvantage where

import Domain.Attribute.TypeOf
import Domain.Attribute.HeldItem
import Domain.Attribute.Damage
import Domain.Damage.DamageOps
import Domain.Pokemon.Species
import Domain.Attribute.Counterparty
import Data.Functor.Contravariant
import Control.Lens

data TypeAdvantage battle = TypeAdvantage
  {
    _typeOfMoveForAdvantage :: battle -> TypeOf
  , _typeAdvantageCalculation :: TypeOf -> TypeOf -> TypeEffect
  , _currentHeldItem :: Counterparty -> battle -> Maybe HeldItem
  , _typeOfPokemonInBattle :: Counterparty -> battle -> TypeOfPokemon
  , _berryAgainstType :: TypeOf -> TypeEffect -> Maybe HeldItem -> Double
  , _supereffectiveItemForUser :: TypeEffect -> Maybe HeldItem -> Double
  }

makeLenses ''TypeAdvantage

instance Contravariant TypeAdvantage where
  contramap f tp = tp {_typeOfMoveForAdvantage = view typeOfMoveForAdvantage tp . f,
                      _currentHeldItem = (\cp -> view currentHeldItem tp cp . f),
                      _typeOfPokemonInBattle = (\cp -> view typeOfPokemonInBattle tp cp . f)}

reduce :: Monad m => DamageOps m -> TypeAdvantage battle -> battle -> m (TypeEffect, Damage)
reduce damops typad btl = do
  let mvt = view typeOfMoveForAdvantage typad btl
  let trt = view typeOfPokemonInBattle typad Target btl
  let ad = foldr (*) 1 .
           fmap effectAsDouble .
           fmap (view typeAdvantageCalculation typad mvt) .
           getTypeOfPokemon $ trt  :: Double
  let eff = case ad of
        ((>= 2) -> True) -> SuperEffective
        ((== 1) -> True) -> NormalEffect
        ((== 0) -> True) -> NoEffect
        _ -> NotVeryEffective
  let tit = view currentHeldItem typad Target btl
  let ber = view berryAgainstType typad mvt eff tit
  -- Jos isku on supertehokas, targetilla on oikea marja ja isku on oikeaa tyyppiä,
  -- niin iskun teho on puolittunut, jolloin marja käytetään
  if ber < 1
    then dropTargetItem damops
    else return ()
  -- Expert Beltin takia
  let uit = view currentHeldItem typad User btl
  let expb = view supereffectiveItemForUser typad eff uit
  return $ (eff, Damage $ ber * ad * expb)
  
