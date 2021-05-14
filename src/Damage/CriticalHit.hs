{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}


module Damage.CriticalHit where

import Damage.Interpreter
import Control.Lens
import Control.Applicative
import Attribute.Ability
import Attribute.Ailment
import Attribute.Item
import Attribute.Counterparty
import Helper.Parse

data CriticalHit = NotCritical | IsCritical deriving (Eq,Show,Ord,Enum)

data CriticalHitLevel =
  Level0
  | Level1
  | Level2
  | Level3
  | Level4
  deriving (Eq,Show,Ord,Enum)

levelToInt :: CriticalHitLevel -> Int
levelToInt = \case
  Level0 -> 0
  Level1 -> 1
  Level2 -> 2
  Level3 -> 3
  Level4 -> 4

type Prob = Double

data CriticalHitCalc a m = CriticalHitCalc
  {
    _initialLevel :: CriticalHitLevel
  , _levelIntToProb :: Int -> Prob
  , _manipulateProb :: Prob -> m Prob
  , _drawHit :: a -> Prob -> m Double
  }

makeLenses ''CriticalHitCalc

mkCriticalHitCalc
  :: DamageOps m
  => (a -> Counterparty -> PokemonAbility)
  -> (a -> Counterparty -> Ailment)
  -> (a -> Counterparty -> HeldItem)
  -> a
  -> CriticalHitCalc a m
mkCriticalHitCalc abfn ailfn itfn dta =
  CriticalHitCalc lvl genVCriticalHit f $ \_ _ -> drawRandomDouble
  where
    f pr = return $ battleArmor abfn dta $ merciless abfn ailfn dta $ pr
    lvl = scopeLens itfn dta $ Level0

reduce :: DamageOps m => CriticalHitCalc a m -> a -> m CriticalHit
reduce calc dta = do
  v <- drawRandomDouble
  let lvl = view levelIntToProb calc . levelToInt $ view initialLevel calc
  lvl' <- view manipulateProb calc lvl
  h <- view drawHit calc dta lvl'
  let res = case (v < h) of
              True  -> IsCritical
              False -> NotCritical
  return res
    

battleArmor :: (a -> Counterparty -> PokemonAbility) -> a -> Prob -> Prob
battleArmor abfn dta prb =
  parseAndAct' m' f dta prb
  where
    m prsm cnstr x = do
      let usrAb = abfn x User
      let trgtAb = abfn x Target
      shllArmr <- preview prsm trgtAb
      notMb <- notMoldBreaker usrAb
      return $ BattleArmorData (cnstr shllArmr) notMb
    m' x = (m _BattleArmor Left x) <|> (m _ShellArmor Right x)
    f _ _ = 0

merciless ::
  (a -> Counterparty -> PokemonAbility) -> (a -> Counterparty -> Ailment) -> a -> Prob -> Prob
merciless abfn ailfn dta prb =
  parseAndAct' m f dta prb
  where
    m x = do
      let usrAb = abfn x User
      let trgtAi = ailfn x Target
      mrclss <- preview _Merciless usrAb
      psnd <- preview _Poisoned trgtAi
      return (mrclss, psnd)
    f _ _ = 1

scopeLens :: (a -> Counterparty -> HeldItem) -> a -> CriticalHitLevel -> CriticalHitLevel
scopeLens itemFn dta lvl =
  parseAndAct' m f dta lvl
  where
    m x = do
      let item = itemFn dta User
      sl <- preview _ScopeLens item
      return $ sl
    f _ = increaseLevel

data BattleArmorData = BattleArmorData (Either BattleArmor ShellArmor) ()

increaseLevel :: CriticalHitLevel -> CriticalHitLevel
increaseLevel Level4 = Level4
increaseLevel x = succ x

genVCriticalHit :: Int -> Prob
genVCriticalHit = \case
  0 -> 0.0625
  1 -> 0.125
  2 -> 0.25
  3 -> 0.333
  _ -> 0.5

genVICriticalHit :: Int -> Prob
genVICriticalHit = \case
  0 -> 0.0625
  1 -> 0.125
  2 -> 0.50
  _ -> 1.00

genVIICriticalHit :: Int -> Prob
genVIICriticalHit = \case
  0 -> 0.04167
  1 -> 0.125
  2 -> 0.5
  _ -> 1.00
