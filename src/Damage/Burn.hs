{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}


module Damage.Burn (
  BurnData()
  ,mkBurnData
  ,currentAilment
  ,currentAbility
  ,moveCategory
  ,ailmentsImpact
  ,nextCriterion
                   ) where

import Control.Lens
import Attribute.Ailment
import Attribute.Counterparty
import Attribute.Ability
import Attribute.Category
import Data.Maybe
import Damage.Interpreter

data BurnData d a (m :: * -> *) = BurnData
  {
    _currentAilment :: a -> Counterparty -> Ailment
  , _currentAbility :: a -> Counterparty -> PokemonAbility
  , _moveCategory :: a -> DamagingCategory'
  , _ailmentsImpact :: Counterparty -> Ailment -> Double
  , _nextCriterion :: d a m
  }

makeLenses ''BurnData

instance InterpretDamage d => InterpretDamage (BurnData d) where
  interp brn dta = do
    nxt <- flip interp dta $ brn ^. nextCriterion
    let ail = (brn ^. currentAilment) dta
    let eff = brn ^. ailmentsImpact
    let usrEff = eff User (ail User)
    let trgtEff = eff Target (ail Target)
    return $ nxt <> Damage usrEff <> Damage trgtEff

mkBurnData ::
  (a -> Counterparty -> Ailment)
  -> (a -> Counterparty -> PokemonAbility)
  -> (a -> DamagingCategory')
  -> d a m
  -> a
  -> BurnData d a m
mkBurnData ail ab cat nxt dta = BurnData ail ab cat f nxt
  where
    g User (BadAilment (Burned _)) = 0.5
    g _ _ = 1.0
    h = preview _PhysicalMove . cat
    f =
      guts ab h dta $
      marvelScale ab h dta $
      g


guts ::
  (a -> Counterparty -> PokemonAbility)
  -> (a -> Maybe Physical)
  -> a
  -> (Counterparty -> Ailment -> Double)
  -> (Counterparty -> Ailment -> Double)
guts getAbility getPhysical dta fn = \cp ai' ->
  let
    ab   = getAbility dta User
    dflt = fn cp ai' 
    res  = do
      gt <- preview _Guts ab
      ph <- getPhysical dta
      ai <- preview _BadAilment ai'
      usr <- isUser cp
      let bse = BurnCase gt () ai ph usr
      return $ (const 1.5) bse
  in fromMaybe dflt res

marvelScale ::
  (a -> Counterparty -> PokemonAbility)
  -> (a -> Maybe Physical)
  -> a
  -> (Counterparty -> Ailment -> Double)
  -> (Counterparty -> Ailment -> Double)
marvelScale getAbility getPhysical dta fn = \cp ai ->
  let
    ab   = getAbility dta User
    ab'  = getAbility dta Target
    dflt = fn cp ai
    res  = do
      ph <- getPhysical dta
      ms <- preview _MarvelScale ab
      mb <- notMoldBreaker ab'
      ai' <- preview _BadAilment ai
      trg <- isTarget cp
      let bse = BurnCase ms mb ai' ph trg
      return $ (const 1.5) bse
    in fromMaybe dflt res


data BurnCase ab  = BurnCase ab () BadAilment' Physical Counterparty
