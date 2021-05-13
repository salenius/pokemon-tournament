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
import Helper.Parse

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
    f =
      guts ab cat dta $
      marvelScale ab cat dta $
      g


data GutsData = GutsData Guts Physical 

data MarvelScaleData = MarvelScaleData MarvelScale Physical

type PhysicalMoveCase a =
 (a -> Counterparty -> PokemonAbility)
  -> (a -> DamagingCategory')
  -> a
  -> (Counterparty -> Ailment -> Double)
  -> (Counterparty -> Ailment -> Double)


guts :: PhysicalMoveCase a
guts getAbility getCategory dta fn =
  parseAndAct' m h dta fn
  where
    m dta = do
      gts <- getAbility dta User & preview _Guts
      phs <- dta & getCategory & preview _PhysicalMove
      return $ GutsData gts phs
    h _ f = \cp ai -> case (cp, preview _BadAilment ai) of
      (User, Just _) -> 1.5
      _              -> f cp ai
      

marvelScale :: PhysicalMoveCase a
marvelScale getAbility getCategory dta fn =
  parseAndAct' m h dta fn
  where
    m dta = do
      msc <- getAbility dta Target & preview _MarvelScale
      phs <- dta & getCategory & preview _PhysicalMove
      return $ MarvelScaleData msc phs
    h _ f = \cp ai -> case (cp, preview _BadAilment ai) of
      (Target, Just _) -> 0.5
      _                -> f cp ai

