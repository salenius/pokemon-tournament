{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}


module Damage.Ratio where

import Control.Lens
import Attribute.Counterparty
import Stats.Base
import Damage.CriticalHit
import Damage.Interpreter
import Data.Maybe

data Ratio = Ratio
  {
    _statModif :: Counterparty -> ModifStat -> Int
  , _statistic :: Counterparty -> BaseStat -> Int
  , _whichCounterparty :: UpOrDown -> Counterparty
  , _whichStat :: UpOrDown -> BaseStat
  , _criticalHitsImpact :: UpOrDown -> CriticalHit -> Int -> Int
  }

class ToRatio a where
  toRatio :: a -> Ratio

data UpOrDown = Down | Up deriving (Eq,Show,Ord)

makeLenses ''Ratio

reduce :: Ratio -> CriticalHit -> Double
reduce r cr =
  let
    upCase   = singleRatioCase Up cr r
    downCase = singleRatioCase Down cr r
  in upCase / downCase

singleRatioCase :: UpOrDown -> CriticalHit -> Ratio -> Double
singleRatioCase ud cr r =
  let
    cp     = (r ^. whichCounterparty) ud
    stat   = (r ^. whichStat) ud
    dfltS' = case ud of
               Up   -> Attack'
               Down -> Defence'
    s'     = fromMaybe dfltS' $ baseToModif stat
    modifL = (r ^. statModif) cp s'
    statAm = (r ^. statistic) cp stat
    crIm   = (r ^. criticalHitsImpact) ud cr modifL
    crI    = modifStatLevelToMultiplier s' crIm
  in crI * fromIntegral statAm
