module Domain.Damage.TypeAdvantage where

import Domain.Logic
import Domain.Common
import Prelude hiding (or,and,not,filter)
import Types.Pokemon
import Types.Effect
import Types.BuiltIn
import qualified Types.BuiltIn as Tp
import Attribute
import Control.Monad.Reader

class (CounterpartyGetter m,
       MoldBreakerGetter m,
       PokemonAttrGetter m,
       Monad m) => TypeAdvantage m where
  moveType :: m TypeOf
  pokemonType :: m PokemonType
  pokemonType = do
    x <- getUser
    getTypeOf x
  isPartOf :: m PokemonType -> TypeOf -> m Bool
  isPartOf pt t = elem t <$> pt
  typeAdvantage :: m TypeOf -> m PokemonType -> m TypeEffect
  isSupereffective :: TypeEffect -> m Bool
  isSupereffective eff = return $ eff == SuperEffective
  moveTypeIs :: TypeOf -> m Bool
  moveTypeIs tp = (==) tp <$> moveType
  multiplyBy :: Double -> m Action
  dropItem :: Player -> m Action
  -- Items
  expertBelt, chilanBerry, babiriBerry :: Player -> m Bool
  occaBerry, chartiBerry, chopleBerry, colburBerry :: Player -> m Bool
  habanBerry, kasibBerry, kebiaBerry, passhoBerry :: Player -> m Bool
  payapaBerry, roseliBerry, rindoBerry, shucaBerry :: Player -> m Bool
  tangaBerry, wacanBerry, yacheBerry  :: Player -> m Bool
  expertBelt = heldItemIs _ExpertBelt
  chilanBerry = heldItemIs _ChilanBerry
  babiriBerry = heldItemIs _BabiriBerry
  occaBerry   = heldItemIs _OccaBerry
  chartiBerry = heldItemIs _ChartiBerry
  chopleBerry = heldItemIs _ChopleBerry
  colburBerry = heldItemIs _ColburBerry
  habanBerry  = heldItemIs _HabanBerry
  kasibBerry  = heldItemIs _KasibBerry
  kebiaBerry  = heldItemIs _KebiaBerry
  passhoBerry = heldItemIs _PasshoBerry
  payapaBerry = heldItemIs _PayapaBerry
  roseliBerry = heldItemIs _RoseliBerry
  rindoBerry  = heldItemIs _RindoBerry
  shucaBerry  = heldItemIs _ShucaBerry
  tangaBerry  = heldItemIs _TangaBerry
  wacanBerry  = heldItemIs _WacanBerry
  yacheBerry  = heldItemIs _YacheBerry
 
  -- Abilities
  neuroforce :: Player -> m Bool
  solidRock :: Player -> m Bool
  filter :: Player -> m Bool

infixl 5 `isPartOf`

program :: TypeAdvantage m => m TypeEffect
program = do
  eff <- typeAdvantage moveType pokemonType
  isSupereffective eff `and` userHas expertBelt
    --> multiplyBy 1.1
  isSupereffective eff
    `and'` targetHas solidRock `or` targetHas filter
    `and'` userHas ~/ moldBreaker'
    --> multiplyBy 0.75
  isSupereffective eff
    `and` userHas neuroforce
    --> multiplyBy 1.25
  targetHas chilanBerry `and` moveTypeIs Tp.Normal
    --> multiplyAndDrop
  whenItComesToBerries eff >-> do
    babiriBerry `iff` Steel
    occaBerry `iff` Fire
    chartiBerry `iff` Rock
    chopleBerry `iff` Fighting
    colburBerry `iff` Dark
    habanBerry `iff` Dragon
    kasibBerry `iff` Ghost
    kebiaBerry `iff` Poison
    passhoBerry `iff` Water
    payapaBerry `iff` Psychic
    roseliBerry `iff` Fairy
    rindoBerry `iff` Grass
    shucaBerry `iff` Ground
    tangaBerry `iff` Bug
    wacanBerry `iff` Electric
    yacheBerry `iff` Ice
  return eff
  where
    iff = berryCase
    (>->) = ($)
    whenItComesToBerries f = flip runReaderT f

multiplyAndDrop :: TypeAdvantage m => m Action
multiplyAndDrop = do
  multiplyBy 0.5
  targetDoes dropItem

berryCase :: TypeAdvantage m =>  (Player -> m Bool) -> TypeOf -> ReaderT TypeEffect m Action
berryCase berrycond tpof = do
  eff <- ask
  lift $ isSupereffective eff
    `and` targetHas berrycond
    `and` moveTypeIs tpof
    --> multiplyAndDrop
