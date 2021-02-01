module Domain.Match.Stat where

import Domain.Attribute.ModifStat
import Domain.Attribute.CriticalHit

criticalHitCancels :: (Ord a, Num a) => (a -> a -> a) -> (a -> b) -> CriticalHit -> a -> b
criticalHitCancels _ f NotCritical a = f a
criticalHitCancels comp f IsCritical a = f . comp 0 $ a

modif2level :: (Int,Int) -> ModifStat -> Int -> Double
modif2level (top,bottom) _ 0 = (fromIntegral top) / (fromIntegral bottom)
modif2level (top,bottom) stat x
  | x > 0 = modif2level (top,bottom - 1) stat $ x - 1
  | x < 0 = modif2level (top-1,bottom) stat $ x + 1

modif4speed = modif2level (8,8) Speed'
modif4accuracy = modif2level (9,9) Accuracy'
modif4evasion = modif2level (9,9) Evasion'
modif4userStat stat = criticalHitCancels max (modif2level (8,8) stat) 
modif4targetStat stat = criticalHitCancels min (modif2level (8,8) stat)
