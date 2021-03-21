{-# LANGUAGE TemplateHaskell #-}

module Domain.Pokemon.Statistic (
  StatisticFactors(..),
  statistic
                                ) where

import Domain.Attribute.PokemonFactors
import Domain.Attribute.Statistic
import Domain.Attribute.Nature
import Control.Lens

data StatisticFactors = StatisticFactors
  {
    _statLevelOf :: Level'
  , _statBase :: BaseStat -> Int
  , _statIV :: BaseStat -> Int
  , _statEV :: BaseStat -> Int
  , _statNature :: Nature
  }

makeLenses ''StatisticFactors

statistic :: BaseStat -> StatisticFactors -> Statistic
statistic BaseHP fs =
  (\amt -> amt + 10 + view statLevelOf fs) .
  normalizeByLevel BaseHP fs .
  baseTimesIV BaseHP fs .
  evOver4 BaseHP $
  fs
statistic stat fs =
  takeNatureToAccount stat fs .
  normalizeByLevel stat fs .
  baseTimesIV stat fs .
  evOver4 stat $ fs

evOver4 :: BaseStat -> StatisticFactors -> Int
evOver4 stat fs = floor $ (fromIntegral $ view statEV fs stat) / 4.0

baseTimesIV :: BaseStat -> StatisticFactors -> Int -> Int
baseTimesIV stat fs amt = 2 * view statBase fs stat + view statIV fs stat + amt

normalizeByLevel :: BaseStat -> StatisticFactors -> Int -> Int
normalizeByLevel stat fs amt =
  floor $ (fromIntegral amt) * (fromIntegral $ view statLevelOf fs) / 100

takeNatureToAccount :: BaseStat -> StatisticFactors -> Int -> Int
takeNatureToAccount stat fs amt =
  floor $ (*) (nature stat $ view statNature fs) $ fromIntegral $ amt + 5
