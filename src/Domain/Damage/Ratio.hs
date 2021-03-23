{-# LANGUAGE TemplateHaskell #-}

module Domain.Damage.Ratio (
  Ratio(),
  mkRatio,
  topOrBottom,
  TopOrBottom(),
  reduce
                           ) where

import Domain.Attribute.Counterparty
import Domain.Attribute.CriticalHit
import Domain.Attribute.ModifStat
import Domain.Attribute.Damage
import Control.Lens
import Data.Functor

data TopOrBottom = Numerator | Denominator

data Ratio battle = Ratio
  {
    _ratioCounterparty :: TopOrBottom -> Counterparty
  , _ratioCalculatedStat :: Counterparty -> battle -> Int
  , _ratioModifiedStat :: Counterparty -> battle -> ModifStatLevel
  , _ratioCriticalHitImpact :: CriticalHit -> Counterparty -> Int -> Int
  }

makeLenses ''Ratio

mkRatio
  :: (Counterparty -> battle -> Int)
  -> (Counterparty -> battle -> ModifStatLevel)
  -> Ratio battle
mkRatio statgetter modifgetter = Ratio defaultCp statgetter modifgetter defaultCrit

combineRatio :: (battle' -> battle) -> Ratio battle -> Ratio battle'
combineRatio f b =
  set ratioCounterparty (view ratioCounterparty b) .
  set ratioCriticalHitImpact (view ratioCriticalHitImpact b) $ mkRatio g g' 
  where
    g = combineFunction f $ view ratioCalculatedStat b
    g' = combineFunction f $ view ratioModifiedStat b


defaultCp Numerator = User
defaultCp Denominator = Target
defaultCrit IsCritical User x = min x 0
defaultCrit IsCritical Target x = max x 0
defaultCrit _ _ x = x

combineFunction :: (a -> b) -> (c -> b -> d) -> (c -> a -> d)
combineFunction tf fn param = fn param . tf

instance Contravariant Ratio where
  contramap = combineRatio

topOrBottom :: Counterparty -> Counterparty -> (TopOrBottom -> Counterparty)
topOrBottom top bot = \s -> case s of
  Numerator -> top
  Denominator -> bot

reduce :: Ratio battle -> battle -> CriticalHit -> Damage
reduce rat b crhit = 
  let
    usr = view ratioCounterparty rat Numerator
    trgt = view ratioCounterparty rat Denominator
    statFn = view ratioCalculatedStat rat
    modifFn = view ratioModifiedStat rat
    crhitFn = view ratioCriticalHitImpact rat
    statTop = statFn usr b
    statBot = statFn trgt b
    mstatTop = modifStatLevelToInt $ modifFn usr b
    mstatBot = modifStatLevelToInt $ modifFn trgt b
    mstatTop' = crhitFn crhit usr mstatTop
    mstatBot' = crhitFn crhit usr mstatBot
    x = modifStatLevelToMultiplier Attack' mstatTop'
    y = modifStatLevelToMultiplier Defence' mstatBot'
    a' = fromIntegral statTop * x
    b' = fromIntegral statBot * y
  in Damage (a' / b')
