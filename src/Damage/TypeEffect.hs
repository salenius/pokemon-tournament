{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Damage.TypeEffect (
  expertBelt
  ,solidRock
  ,neuroforce
  ,filter
  ,allBerries
  ,mkTypeEffect
  ,TypeEffect()
  ,BerryData(..)
  ,calcTypeAdvantage
  ,doubleToAdvantage
  ,pokemonType
  ,moveType
  ,supereffectiveCase
  ,nextCriterion
                         ) where

import qualified Types.Effect as Eff
import Types.Pokemon
import Attribute.Counterparty
import Attribute.Item
import Attribute.Ability
import Damage.Interpreter
import Data.Maybe
import Prelude hiding (filter)

type DamageEffect a m = a -> Eff.TypeEffect -> m Double


data TypeEffect d a m = TypeEffect
  {
    _calcTypeAdvantage :: Eff.TypeOf -> Eff.TypeOf -> Eff.TypeEffect
  , _doubleToAdvantage :: Double -> Eff.TypeEffect
  , _pokemonType :: a -> Counterparty -> PokemonType
  , _moveType :: a -> Eff.TypeOf
  , _supereffectiveCase :: DamageEffect a m
  , _nextCriterion :: d a m
  }

makeLenses ''TypeEffect

mkTypeEffect ::
  (a -> Counterparty -> PokemonType)
  -> (a -> Eff.TypeOf)
  -> DamageEffect a m
  -> d a m
  -> TypeEffect d a m
mkTypeEffect = TypeEffect Eff.advantage f
  where
    f 1.0 = Eff.NormalEffect
    f 0.0 = Eff.NoEffect
    f x
      | x < 1.0 = Eff.NotVeryEffective
      | otherwise = Eff.SuperEffective

type SingleAttributeEff ab a m =
  (a -> Counterparty -> Maybe ab)
  -> DamageEffect a m
  -> DamageEffect a m

type Berry' a m =
  (a -> BerryData)
  -> DamageEffect a m
  -> DamageEffect a m


expertBelt :: DamageOps m => SingleAttributeEff ExpertBelt a m
expertBelt = superEffectiveCase ((*) 1.3) User

solidRock :: DamageOps m => SingleAttributeEff SolidRock a m
solidRock = superEffectiveCase ((*) 0.75) Target

filter :: DamageOps m => SingleAttributeEff Filter a m
filter = superEffectiveCase ((*) 0.75) Target


neuroforce :: DamageOps m => SingleAttributeEff Neuroforce a m
neuroforce = superEffectiveCase ((*) 1.25) User

allBerries :: DamageOps m => Berry' a m
allBerries =
  babiriBerry €
  chilanBerry €
  occaBerry €
  chartiBerry €
  chopleBerry €
  colburBerry €
  cobaBerry €
  habanBerry €
  kasibBerry €
  kebiaBerry €
  passhoBerry €
  payapaBerry €
  roseliBerry €
  rindoBerry €
  shucaBerry €
  tangaBerry €
  wacanBerry €
  yacheBerry

instance InterpretDamage d => InterpretDamage (TypeEffect d) where
  interp teff dta = do
    othr           <- flip interp dta $ view nextCriterion teff
    let mtype      = view moveType teff dta
    let targetType = view pokemonType teff dta User
    let advtgeCalc = view calcTypeAdvantage teff mtype
    let advNumber  = foldr (*) 1 .
          fmap Eff.effectAsDouble .
          fmap advtgeCalc $
          targetType
    let advtge     = view doubleToAdvantage teff advNumber
    extraEff       <- view supereffectiveCase teff dta advtge
    return $ othr <> Damage advNumber <> Damage extraEff


superEffectiveCase :: DamageOps m =>
  (Double -> Double) -> Counterparty -> SingleAttributeEff ab a m
superEffectiveCase rule cp parser damEff dta eff = do
  let abIt = parser dta cp
  let efIs = case eff of
               Eff.SuperEffective -> True
               _                  -> False
  amt     <- damEff dta eff
  let mult = case (abIt, efIs) of
               (Just _, True) -> rule amt
               _              -> amt
  return mult

type Berry bry a m = (HeldItem -> Maybe bry) ->  Berry' a m


data BerryData = BerryData
  {
    berryDataHeldItem :: Counterparty -> HeldItem
  , berryDataMoveType :: Eff.TypeOf
  }


type BerryTypeAdvantage bry = bry -> Eff.TypeEffect -> Double
type BerryMoveType bry = bry -> Eff.TypeOf -> Double

berry :: DamageOps m => BerryTypeAdvantage bry -> BerryMoveType bry -> Berry bry a m
berry advtg mtype berryParser getDta superadv dta' eff = do
  let dta = getDta dta'
  x <- superadv dta' eff
  -- Hae käytössä oleva marja ja liikkeen tyyppi
  let br = berryParser $ berryDataHeldItem dta Target
  let mt = berryDataMoveType dta
  -- Laske sen vaikutus pitääkö olla supertehokas
  let effMult = fromMaybe 1.0 $ flip advtg eff <$> br
  -- Laske vaikutus sille mitä tyyppiä iskun pitää olla
  let mvTMult = fromMaybe 1.0 $ flip mtype mt <$> br
  let total = max 0.5 (effMult * mvTMult)
  case total of
    0.5 -> dropItem Target
    _   -> return ()
  return $ x * total
 

halfIfSuperEffective :: BerryTypeAdvantage bry
halfIfSuperEffective = \_ eff -> if eff == Eff.SuperEffective then 0.5 else 1.0

halfIfTypeOf :: Eff.TypeOf -> BerryMoveType bry
halfIfTypeOf t = \_ tp -> if tp == t then 0.5 else 1.0

superBerry t = berry halfIfSuperEffective (halfIfTypeOf t)

babiriBerry :: DamageOps m => Berry' a m
babiriBerry = superBerry Eff.Steel (preview _BabiriBerry)

chilanBerry :: DamageOps m => Berry' a m
chilanBerry = berry (\_ _ -> 0.5) (halfIfTypeOf Eff.Normal) (preview _ChilanBerry)
    
occaBerry :: DamageOps m => Berry' a m
occaBerry = superBerry Eff.Fire (preview _OccaBerry)

chartiBerry :: DamageOps m => Berry' a m
chartiBerry = superBerry Eff.Rock (preview _ChartiBerry)

chopleBerry :: DamageOps m => Berry' a m
chopleBerry = superBerry Eff.Fighting (preview _ChopleBerry)

cobaBerry :: DamageOps m => Berry' a m
cobaBerry = superBerry Eff.Flying (preview _CobaBerry)

colburBerry :: DamageOps m => Berry' a m
colburBerry = superBerry Eff.Dark (preview _ColburBerry)

habanBerry :: DamageOps m => Berry' a m
habanBerry = superBerry Eff.Dragon (preview _HabanBerry)

kasibBerry :: DamageOps m => Berry' a m
kasibBerry = superBerry Eff.Ghost (preview _KasibBerry)

kebiaBerry :: DamageOps m => Berry' a m
kebiaBerry = superBerry Eff.Poison (preview _KebiaBerry)

passhoBerry :: DamageOps m => Berry' a m
passhoBerry = superBerry Eff.Water (preview _PasshoBerry)

payapaBerry :: DamageOps m => Berry' a m
payapaBerry = superBerry Eff.Psychic (preview _PayapaBerry)

roseliBerry :: DamageOps m => Berry' a m
roseliBerry = superBerry Eff.Fairy (preview _RoseliBerry)

rindoBerry :: DamageOps m => Berry' a m
rindoBerry = superBerry Eff.Grass (preview _RindoBerry)

shucaBerry :: DamageOps m => Berry' a m
shucaBerry = superBerry Eff.Ground (preview _ShucaBerry)

tangaBerry :: DamageOps m => Berry' a m
tangaBerry = superBerry Eff.Bug (preview _TangaBerry)

wacanBerry :: DamageOps m => Berry' a m
wacanBerry = superBerry Eff.Electric (preview _WacanBerry)

yacheBerry :: DamageOps m => Berry' a m
yacheBerry = superBerry Eff.Ice (preview _YacheBerry)

(€) :: DamageOps m => Berry' a m -> Berry' a m -> Berry' a m
(€) berry1 berry2 getBerryData gen =
  berry1 getBerryData gen `combineEffect` berry2 getBerryData gen

 
combineEffect :: DamageOps m => DamageEffect a m -> DamageEffect a m -> DamageEffect a m
combineEffect gen1 gen2 dta tpeff = do
  a <- gen1 dta tpeff
  b <- gen2 dta tpeff
  return $ a * b
