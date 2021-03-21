{-# LANGUAGE TemplateHaskell #-}

module Domain.Attribute.PokemonFactors where

import Domain.Attribute.Ability
import Domain.Attribute.HeldItem
import Domain.Attribute.Gender
import Domain.Attribute.Nature
import Control.Lens

type IV = Int
type EV = Int
type Level' = Int

data IVs = IVs
  {
    _hpIv :: IV
  , _attackIv :: IV
  , _defenceIv :: IV
  , _sAttackIv :: IV
  , _sDefenceIv :: IV
  , _speedIv :: IV
  } deriving (Eq,Show,Read)

makeLenses ''IVs

data EVs = EVs
  {
    _hpEv :: EV
  , _attackEv :: EV
  , _defenceEv :: EV
  , _sAttackEv :: EV
  , _sDefenceEv :: EV
  , _speedEv :: EV
  } deriving (Eq,Show,Read)

makeLenses ''EVs

data IndividualFactor = IndividualFactor
  {
    ivs :: IVs
  , evs :: EVs
  , natureOf' :: Nature
  , levelOf' :: Level'
  } deriving (Eq,Show)

defaultIVs = IVs 31 31 31 31 31 31
defaultEVs = EVs 85 85 85 85 85 85

defaultLevel:: Level'
defaultLevel = 50

defaultNature = Timid
