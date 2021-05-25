module Damage.Interpreter (
  module DData,
  module CLens,
  InterpretDamage,
  interp,
  DamageOps,
  logInfo,
  dropItem,
  drawRandomDouble,
  drawRandomInteger
                          ) where

import Damage.Data as DData
import Attribute.Counterparty
import Control.Lens as CLens

class InterpretDamage d where
  interp :: DamageOps m => d m -> m Damage

class Monad m => DamageOps m where
  logInfo :: String -> m ()
  dropItem :: Counterparty -> m ()
  drawRandomDouble :: m Double
  drawRandomInteger :: Int -> Int -> m Int
