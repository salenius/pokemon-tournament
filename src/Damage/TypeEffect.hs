{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}


module Damage.TypeEffect (
  expertBelt
  ,solidRock
  ,neuroforce
  ,filter
  ,mkTypeEffect
  ,compoundBerries
  ,TypeEffect()
  ,calcTypeAdvantage
  ,doubleToAdvantage
  ,pokemonType
  ,pokemonAbility
  ,pokemonHeldItem
  ,moveType
  ,supereffectiveCase
  ,nextCriterion
                         ) where

import qualified Types.Effect as Eff
import Types.Pokemon
import Attribute.Counterparty
import Attribute.Item
import Attribute.Ability
import Attribute.TypeOf
import Control.Monad.Reader
import Damage.Interpreter
import Data.Maybe
import Prelude hiding (filter)

type DamageEffect = Eff.TypeEffect -> Double -> Double


data TypeEffect d m = TypeEffect
  {
    _calcTypeAdvantage :: Eff.TypeOf -> Eff.TypeOf -> Eff.TypeEffect
  , _doubleToAdvantage :: Double -> Eff.TypeEffect
  , _pokemonType ::  Counterparty -> PokemonType
  , _pokemonHeldItem :: Counterparty -> Maybe HeldItem
  , _pokemonAbility :: Counterparty -> PokemonAbility
  , _moveType :: Eff.TypeOf
  , _supereffectiveCase :: Eff.TypeEffect -> Double -> m Double
  , _nextCriterion :: d m
  }

makeLenses ''TypeEffect


mkTypeEffect ::
  (DamageOps m)
  => (Counterparty -> PokemonType)
  -> (Counterparty -> Maybe HeldItem)
  -> (Counterparty -> PokemonAbility)
  -> Eff.TypeOf
  -> d m
  -> TypeEffect d m
mkTypeEffect pkType pkItem pkAbil mvType nxt =
  TypeEffect Eff.advantage f pkType pkItem pkAbil mvType g nxt
  where
    f 1 = Eff.NormalEffect
    f 0 = Eff.NoEffect
    f x
      | x < 1 = Eff.NotVeryEffective
      | otherwise = Eff.SuperEffective
    g eff amt = do
      brs <- compoundBerries (\cp -> (mvType, pkItem cp)) eff amt
      return $
        expertBelt pkItem eff $
        solidRock pkAbil eff $
        filter pkAbil eff $
        neuroforce pkAbil eff $
        brs

-----


type EffectParse b = Maybe b -> (Double -> Double) -> DamageEffect

expertBelt ab = supereffectiveBoost (preview _ExpertBelt <$> ab User) ((*) 1.1)

solidRock ab = supereffectiveBoost (preview _SolidRock $ ab Target) ((*) 0.75)

filter ab = supereffectiveBoost (preview _Filter $ ab Target) ((*) 0.75)

neuroforce ab = supereffectiveBoost (preview _Neuroforce $ ab User) ((*) 0.75) 

effectiveBoost :: (Eff.TypeEffect -> Bool) -> EffectParse b 
effectiveBoost tpeff pr nval = \eff amt ->
  let x = pr
  in case (x, tpeff eff) of
       (Just _, True) -> nval amt * amt
       _              -> amt

-- supereffectiveBoost :: DamageOps m => EffectParse a b m
supereffectiveBoost = effectiveBoost ((==) Eff.SuperEffective)

-----


chilanBerry = berry (effectiveBoost (const True)) _NormalType _ChilanBerry 

babiriBerry = superBerry _SteelType _BabiriBerry
    
occaBerry = superBerry _FireType _OccaBerry

chartiBerry = superBerry _RockType _ChartiBerry

chopleBerry = superBerry _FightingType _ChopleBerry

cobaBerry = superBerry _FlyingType _CobaBerry

colburBerry = superBerry _DarkType _ColburBerry

habanBerry = superBerry _DragonType _HabanBerry

kasibBerry = superBerry _GhostType _KasibBerry

kebiaBerry = superBerry _PoisonType _KebiaBerry

passhoBerry = superBerry _WaterType _PasshoBerry

payapaBerry = superBerry _PsychicType _PayapaBerry

roseliBerry = superBerry _FairyType _RoseliBerry

rindoBerry = superBerry _GrassType _RindoBerry

shucaBerry = superBerry _GroundType _ShucaBerry

tangaBerry = superBerry _BugType _TangaBerry

wacanBerry = superBerry _ElectricType _WacanBerry

yacheBerry = superBerry _IceType _YacheBerry


berry ::
  (Maybe (a,b) -> (Double -> Double) -> DamageEffect)
  -> Prism' Eff.TypeOf a
  -> Prism' HeldItem b
  -> (Counterparty -> (Eff.TypeOf, Maybe HeldItem))
  -> DamageEffect
berry f typePrism berryPrism getter = f (g . getter $ Target) ((*) 0.5) 
  where
    g = \(x,x') -> do
      y <- preview typePrism x
      z <- x' >>= preview berryPrism
      return $ (y,z)

superBerry = berry supereffectiveBoost

type BerryTf = (Counterparty -> (Eff.TypeOf, Maybe HeldItem)) -> DamageEffect

type BerryEnv = forall m. (DamageOps m) => ReaderT (Counterparty -> (Eff.TypeOf, Maybe HeldItem), Eff.TypeEffect) m Double

asBerryEnv :: BerryTf -> Double -> BerryEnv
asBerryEnv btf = btf & uncurry & reader & (??)

dropItemIf :: BerryTf -> Double -> BerryEnv
dropItemIf orig = \d -> do
  x <- asBerryEnv orig d
  let lessthan = x < d
  case lessthan of
    True -> lift $ dropItem Target
    False -> return ()
  return x

(€) :: (Double -> BerryEnv) -> BerryTf -> Double -> BerryEnv
a € b = \d -> a d >>= dropItemIf b

allBerries :: Double -> BerryEnv
allBerries =
  dropItemIf babiriBerry € 
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

compoundBerries :: (DamageOps m) => (Counterparty -> (Eff.TypeOf, Maybe HeldItem)) -> Eff.TypeEffect -> Double -> m Double
compoundBerries cpf tpeff amt = runReaderT (allBerries amt) (cpf, tpeff)

-----

{--

instance InterpretDamage d => InterpretDamage (TypeEffect d) where
  interp teff = do
    othr           <- interp $ view nextCriterion teff
    let mtype      = view moveType teff 
    let targetType = view pokemonType teff User
    let advtgeCalc = view calcTypeAdvantage mtype
    let advNumber  = foldr (*) 1 .
          fmap Eff.effectAsDouble .
          fmap advtgeCalc $
          targetType
    let advtge     = view doubleToAdvantage teff advNumber
    extraEff       <- view supereffectiveCase teff advtge
    return $ othr <> Damage advNumber <> Damage extraEff


 
combineEffect :: DamageOps m => DamageEffect a m -> DamageEffect a m -> DamageEffect a m
combineEffect gen1 gen2 dta tpeff = do
  a <- gen1 dta tpeff
  b <- gen2 dta tpeff
  return $ a * b
--}
