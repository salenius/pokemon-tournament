{-# LANGUAGE TemplateHaskell, ViewPatterns, PatternSynonyms #-}

module Domain.Attribute.HP (
  HP(),
  mkHp,
  pattern HP,
  addHp,
  addHpPct,
  hpPct,
  hpPctLessThan,
  maxHp
                           ) where

import Control.Lens

data HP = MkHP
  {
    _currentHp :: Int,
    _maxHp :: Int
  }

makeLenses ''HP

pattern HP hp <- MkHP hp _

mkHp :: Int -> HP
mkHp x = MkHP x' x'
  where
    x' = max 0 x

setHp :: Int -> HP -> HP
setHp new old =
  set currentHp new' old
  where
    new' = min (view maxHp old) . max 0 $ new

addHp :: Int -> HP -> HP
addHp new old = setHp ((view currentHp old) + new) old

addHpPct :: Double -> HP -> HP
addHpPct pct hp = addHp (floor $ pct * (fromIntegral $ view maxHp hp)) hp

hpPct :: HP -> Double
hpPct hp = fromIntegral cur / fromIntegral max'
  where
    cur = view currentHp hp
    max' = view maxHp hp

hpPctLessThan :: Double -> HP -> Bool
hpPctLessThan lim = (<) lim . hpPct

instance Eq HP where
  (==) a b = view currentHp a == view currentHp b

instance Show HP where
  show hp = "HP: " ++ (show . view currentHp $ hp) ++ " / " ++ (show . view maxHp $ hp)

