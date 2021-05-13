{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}


module Damage.Stab where

import Control.Lens
import Attribute.Counterparty
import Types.Pokemon
import Types.BuiltIn
import Damage.Interpreter

data Stab d a (m :: * -> *) = Stab
  {
    _pokemonType :: a -> Counterparty -> PokemonType
  , _moveType :: a -> TypeOf
  , _stabMultiplier :: Double
  , _nextCondition :: d a m
  }

makeClassy ''Stab

defaultStab :: (a -> Counterparty -> PokemonType) -> (a -> TypeOf) -> d a m -> Stab d a m
defaultStab pt mt nc = Stab pt mt 1.5 nc

instance InterpretDamage d => InterpretDamage (Stab d) where
  interp stab dta = do
    nxt      <- flip interp dta (stab ^. nextCondition) 
    let pType = (stab ^. pokemonType) dta User
    let mType = (stab ^. moveType) dta
    let isIn  = mType `elem` pType
    let mult  = case isIn of
                  True -> stab ^. stabMultiplier
                  False -> 1.0
    return $ nxt <> Damage mult 
    
