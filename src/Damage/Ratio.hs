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

data Ratio a = Ratio
  {
    _statModif :: a -> Counterparty -> ModifStat -> Int
  , _statistic :: a -> Counterparty -> BaseStat -> Int
  , _whichCounterparty :: UpOrDown -> Counterparty
  , _whichStat :: UpOrDown -> BaseStat
  , _criticalHitsImpact :: UpOrDown -> CriticalHit -> Int -> Int
  }

data UpOrDown = Down | Up deriving (Eq,Show,Ord)

makeLenses ''Ratio

reduce :: Ratio a -> CriticalHit -> a -> Double
reduce r cr d =
  let
    upCase   = singleRatioCase Up cr r d
    downCase = singleRatioCase Down cr r d
  in upCase / downCase

singleRatioCase :: UpOrDown -> CriticalHit -> Ratio a -> a -> Double
singleRatioCase ud cr r d =
  let
    cp     = (r ^. whichCounterparty) ud
    stat   = (r ^. whichStat) ud
    dfltS' = case ud of
               Up   -> Attack'
               Down -> Defence'
    s'     = fromMaybe dfltS' $ baseToModif stat
    modifL = (r ^. statModif) d cp s'
    statAm = (r ^. statistic) d cp stat
    crIm   = (r ^. criticalHitsImpact) ud cr modifL
    crI    = modifStatLevelToMultiplier s' crIm
  in crI * fromIntegral statAm
