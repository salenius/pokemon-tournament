{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}


module Stats.Calc (
  StatisticFactors(..)
  ,ModifiedStatisticFactors
  ,HasStatisticFactors
  ,statisticFactors
  ,level
  ,baseStat
  ,nature
  ,evStat
  ,ivStat
  ,statistic
  ,getMaxHp
  ,getAttack
  ,getDefence
  ,getSAttack
  ,getSDefence
  ,getSpeed
  ,getStatMultiplier
  ,withModifications
                  ) where

import Stats.Nature
import Stats.Base
import Stats.Level as Sl
import Stats.EV
import Stats.IV
import GHC.Generics
import Control.Lens
import Data.Map as Map
import Data.Maybe (fromMaybe)

data StatisticFactors modif = StatisticFactors
  {
    _level :: Sl.Level
  , _baseStat :: BaseStatMap BaseStatValue
  , _nature :: Nature
  , _evStat :: BaseStatMap EV
  , _ivStat :: BaseStatMap IV
  , _modifications :: modif
  } deriving (Eq,Show,Ord,Generic,Functor)

makeClassy ''StatisticFactors

type Statistic = Int

type ModifiedStatisticFactors = StatisticFactors (Map.Map ModifStat ModifStatLevel)

class StatCalc f where
  statistic :: BaseStat -> f -> Double

instance StatCalc (StatisticFactors ()) where
  statistic b = fromIntegral . statistic' b

instance StatCalc ModifiedStatisticFactors where
  statistic = statistic''

getMaxHp :: StatCalc f => f -> Int
getMaxHp = floor . statistic BaseHP

getAttack :: StatCalc f => f -> Double
getAttack = statistic BaseAttack

getDefence :: StatCalc f => f -> Double
getDefence = statistic BaseDefence

getSAttack :: StatCalc f => f -> Double
getSAttack = statistic BaseSAttack

getSDefence :: StatCalc f => f -> Double
getSDefence = statistic BaseSDefence

getSpeed :: StatCalc f => f -> Double
getSpeed = statistic BaseSpeed

-- Lasketaan perustilasto kun otetaan kaikki tekijät huomioon
statistic' :: BaseStat -> StatisticFactors m -> Statistic
statistic' BaseHP fs =
  (\amt -> amt + 10 + (levelToInt $ view level fs)) .
  normalizeByLevel BaseHP fs .
  baseTimesIV BaseHP fs .
  evOver4 BaseHP $
  fs
statistic' stat fs =
  takeNatureToAccount stat fs .
  normalizeByLevel stat fs .
  baseTimesIV stat fs .
  evOver4 stat $ fs

-- Lasketaan perustilasto ja otetaan lisäksi huomioon mahdollinen modifier
statistic'' :: BaseStat -> ModifiedStatisticFactors -> Double
statistic'' stat fct = a * fromIntegral b
  where
    b = statistic' stat fct
    a = statModifier id stat fct

statModifier :: (Int -> Int) -> BaseStat -> ModifiedStatisticFactors -> Double
statModifier tweekInt stat fct =
  fromMaybe 1.0 $ do
   bstat    <- baseToModif stat 
   lvl      <- Map.lookup bstat (view modifications fct)
   let lvl'  = tweekInt . modifStatLevelToInt $ lvl
   let rest  = modifStatLevelToMultiplier bstat lvl'
   return rest

getStatMultiplier = statModifier

-- Transform StatisticFactors with(out) modifications into a one with modifications
withModifications :: [(ModifStat, ModifStatLevel)] -> StatisticFactors m -> ModifiedStatisticFactors
withModifications lst = fmap (\_ -> Map.fromList lst)

--- Helper functions

evOver4 :: BaseStat -> StatisticFactors m -> Int
evOver4 stat fs = floor $ (fromIntegral $ evToInt $ helper' ev' stat) / 4.0
  where
    ev' = view evStat fs

baseTimesIV :: BaseStat -> StatisticFactors m -> Int -> Int
baseTimesIV stat fs amt = 2 * (baseStatValueToInt $ helper' base' stat) + (ivToInt $ helper' iv' stat) + amt
  where
    iv' = view ivStat fs
    base' = view baseStat fs

normalizeByLevel :: BaseStat -> StatisticFactors m -> Int -> Int
normalizeByLevel stat fs amt =
  floor $ (fromIntegral amt) * (fromIntegral $ levelToInt $ view level fs) / 100

takeNatureToAccount :: BaseStat -> StatisticFactors m -> Int -> Int
takeNatureToAccount stat fs amt =
  floor $ (*) (natureImpact stat $ view nature fs) $ fromIntegral $ amt + 5

natureImpact :: BaseStat -> Nature -> Double
natureImpact BaseAttack Lonely = 1.1
natureImpact BaseAttack Brave = 1.1
natureImpact BaseAttack Adamant = 1.1
natureImpact BaseAttack Naughty = 1.1
natureImpact BaseAttack Bold = 0.9
natureImpact BaseAttack Timid = 0.9
natureImpact BaseAttack Modest = 0.9
natureImpact BaseAttack Calm = 0.9
natureImpact BaseDefence Bold = 1.1
natureImpact BaseDefence Relaxed = 1.1
natureImpact BaseDefence Impish = 1.1
natureImpact BaseDefence Lax = 1.1
natureImpact BaseDefence Lonely = 0.9
natureImpact BaseDefence Hasty = 0.9
natureImpact BaseDefence Mild = 0.9
natureImpact BaseDefence Gentle = 0.9
natureImpact BaseSAttack Modest = 1.1
natureImpact BaseSAttack Mild = 1.1
natureImpact BaseSAttack Quiet = 1.1
natureImpact BaseSAttack Rash = 1.1
natureImpact BaseSAttack Adamant = 0.9
natureImpact BaseSAttack Impish = 0.9
natureImpact BaseSAttack Jolly = 0.9
natureImpact BaseSAttack Careful = 0.9
natureImpact BaseSDefence Calm = 1.1
natureImpact BaseSDefence Gentle = 1.1
natureImpact BaseSDefence Sassy = 1.1
natureImpact BaseSDefence Careful = 1.1
natureImpact BaseSDefence Naughty = 0.9
natureImpact BaseSDefence Lax = 0.9
natureImpact BaseSDefence Naive = 0.9
natureImpact BaseSDefence Rash = 0.9
natureImpact BaseSpeed Timid = 1.1
natureImpact BaseSpeed Hasty = 1.1
natureImpact BaseSpeed Jolly = 1.1
natureImpact BaseSpeed Naive = 1.1
natureImpact BaseSpeed Brave = 0.9
natureImpact BaseSpeed Relaxed = 0.9
natureImpact BaseSpeed Quiet = 0.9
natureImpact BaseSpeed Sassy = 0.9
natureImpact _ _ = 1


helper' = baseStatMapAsFunction
