{-# LANGUAGE TemplateHaskell #-}

module Domain.Damage.Core where

import Control.Lens
import Domain.Damage.DamageOps as DOps
import Domain.Damage.BaseDamage as BD
import Domain.Damage.Ratio as R
import Domain.Damage.TypeAdvantage as TA
import Domain.Damage.Stab as Stab
import Domain.Damage.Weather as WD
import Domain.Damage.CriticalHit as CH
import Domain.Damage.Info as I
import Domain.Attribute.Statistic as St
import Domain.Attribute.Damage
import Domain.Attribute.Counterparty

data TotalDamage battle = TotalDamage
  {
    _damageLevelOfPokemon :: Counterparty -> battle -> St.Level
  , _damageBasepower :: BaseDamageMove battle
  , _damageRatio :: Ratio battle
  , _damageStab :: Stab battle
  , _damageTypeAdvantage :: TypeAdvantage battle
  , _damageWeatherEffect :: WeatherDamage battle
  , _damageCriticalHitEffect :: CriticalHitDamage battle
  }

data DamageEffs monad battle = DamageEffs
  {
    _randomDouble :: monad Double
  , _damageOps :: DOps.DamageOps monad
  , _criticalHitEffs :: CH.CriticalHitCalc monad battle
  }

makeLenses ''TotalDamage
makeLenses ''DamageEffs

reduceTotal
  :: Monad m
  => DamageEffs m battle
  -> TotalDamage battle
  -> battle
  -> m I.DamageInfo
reduceTotal effs dam battle = do
  crhit <- CH.produceCriticalHit (view criticalHitEffs effs) battle
  let basep = BD.reduce (view damageBasepower dam) battle
  let ratio = R.reduce (view damageRatio dam) battle crhit
  let base = baseDamage (view damageLevelOfPokemon dam User battle) $ basep <> ratio
  let stab = Stab.reduce (view damageStab dam) battle
  (typeff', typeff) <- TA.reduce (view damageOps effs) (view damageTypeAdvantage dam) battle
  let creff = CH.reduce (view damageCriticalHitEffect dam) battle
  let wthr = WD.reduce (view damageWeatherEffect dam) battle
  rand <- view randomDouble effs
  let damAquired = base <> stab <> typeff <> wthr <> creff <> Damage rand
  return $ DamageInfo damAquired crhit typeff'

baseDamage :: St.Level -> Damage -> Damage
baseDamage lvl dam =
  let
    lvl' = fromIntegral $ 2 * lvl
    lvl'' = lvl' / 5
    a = lvl'' * fromDamage dam
    b = a / 50
    c = b + 2
    in Damage c
