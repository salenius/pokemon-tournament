{-# LANGUAGE TemplateHaskell #-}

module Domain.Attribute.HP where

import Control.Lens

data HP = HP
  {
    _currentHp :: Int,
    _maxHp :: Int
  }

makeLenses ''HP

mkHp :: Int -> HP
mkHp x = HP x' x'
  where
    x' = max 0 x

setHp :: Int -> HP -> HP
setHp new old =
  set currentHp new' old
  where
    new' = min (view maxHp old) . max 0 $ new

addHp :: Int -> HP -> HP
addHp new old = setHp ((view currentHp old) + new) old

hpPct :: HP -> Double
hpPct hp = fromIntegral cur / fromIntegral max'
  where
    cur = view currentHp hp
    max' = view maxHp hp

instance Eq HP where
  (==) a b = view currentHp a == view currentHp b

instance Show HP where
  show hp = "HP: " ++ (show . view currentHp $ hp) ++ " / " ++ (show . view maxHp $ hp)

