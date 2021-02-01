{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

module Domain.Entity.PokemonState where

import Control.Lens
import Domain.Attribute.Weather as W
import Domain.Attribute.TypeOf as Tp
import Domain.Attribute.Player
import Domain.Attribute.HeldItem
import Domain.Attribute.Ability
import Domain.Attribute.Ailment
import Domain.Attribute.HP as HP
import Domain.Entity.Move as M
import Domain.Entity.Pokemon as P
import Domain.Entity.Pokemon.Species as PS
import Domain.Entity.Trainer
import Domain.Attribute.ModifStat
import Domain.Attribute.Statistic
import qualified Data.Map as Map

data StatusAilment =
  IsHealthy
  | IsParalyzed
  | IsBurned
  | IsPoisoned
  | IsFrozen Int
  | IsSleep Int
  deriving (Eq,Show)

makePrisms ''StatusAilment

data CurrentStatus = CurrentStatus
  {
    _statModifications :: Map.Map ModifStat Int
  , _ailment :: StatusAilment
  , _poisonMultiplier :: Int
  , _confused :: Confused
  , _flinched :: Flinched
  , _leechSeeded :: LeechSeeded
  , _protected :: Protected
  , _yawned :: Yawned
  } deriving (Eq,Show)

makeLenses ''CurrentStatus

data CurrentAttributes = CurrentAttributes
  {
    _ability :: Ability
  , _heldItem :: Maybe HeldItem
  , _typeOf :: [TypeOf]
  } deriving (Eq,Show)

makeLenses ''CurrentAttributes

data DegreesOfFreedom =
  FreeToUseAnything
  | LockedPermanently Move
  | LockedTemporarily Move Int
  deriving (Eq, Show)

data MoveUsageState = MoveUsageState
  {
    _powerPointsLeft :: Quadruple (Move,Int)
  , _moveUseHistory :: [Move]
  , _moveUseDegreesOfFreedom :: DegreesOfFreedom
  } deriving (Eq, Show)

makeLenses ''MoveUsageState

data PokemonState = PokemonState
  {
    _pokemonIndividual :: Pokemon Move
  , _hpAmount :: HP
  , _currentStatus :: CurrentStatus
  , _currentAttributes :: CurrentAttributes
  , _moveUsageState :: MoveUsageState
  } deriving (Eq,Show)

makeLenses ''PokemonState

statistic :: BaseStat -> Lens' PokemonState Int
statistic basestat = lens (\p -> P.newStat basestat $ view pokemonIndividual p) const

getStatistic basestat = view (statistic basestat)

newPokemonState :: Pokemon Move -> PokemonState
newPokemonState pkmn =
  PokemonState pkmn (mkHp $ newStat BaseHP pkmn) statusCur attrsCur usage
  where
    statusCur = CurrentStatus (Map.fromList []) IsHealthy 0 NotConfused NotFlinched NotLeechSeeded NotProtected NotYawned
    attrsCur = CurrentAttributes (head $ view possibleAbilities pkmn) (Just . view possibleHeldItem $ pkmn) $
      view (P.species . PS.typeOf) pkmn
    usage =
      MoveUsageState (fmap (\p -> (p, view (necessities . M.species . M.powerPoints) p)) (view moves pkmn)) [] FreeToUseAnything
  
-- Operaatiot

addHpAmount :: Int -> PokemonState -> PokemonState
addHpAmount amount = over hpAmount (HP.addHp amount)

addHpPercent :: Double -> PokemonState -> PokemonState
addHpPercent pct = over hpAmount (f pct)
  where
    f pct hp = flip addHp hp $ floor $ pct * (fromIntegral $ view maxHp hp)

dropItem :: PokemonState -> PokemonState
dropItem = over (currentAttributes . heldItem) (\_ -> Nothing)

setAilmentForTurns :: Ailment -> Int -> PokemonState -> PokemonState
setAilmentForTurns ailm trns =
  over (currentStatus . ailment . _IsFrozen) (\_ -> trns) .
  over (currentStatus . ailment . _IsSleep) (\_ -> trns) .
  over (currentStatus . ailment) (f ailm)
  where
    f Healthy _ = IsHealthy
    f Poisoned _ = IsPoisoned
    f Burned _ = IsBurned
    f Sleep _ = IsSleep 0
    f Frozen _ = IsFrozen 0
    
setPoisoned = setAilmentForTurns Poisoned 0 . over (currentStatus . poisonMultiplier)(+1)
setBurned = setAilmentForTurns Burned 0
setParalyzed = setAilmentForTurns Paralyzed 0
setFrozen trns = setAilmentForTurns Frozen trns
setSleep trns = setAilmentForTurns Sleep trns

addStatModification :: ModifStat -> Int -> PokemonState -> PokemonState
addStatModification stat level pkmn =
  let statmap = view (currentStatus . statModifications) pkmn
      plus x y = min 6 . max (-6) $ x + y
      wanted = Map.lookup stat statmap 
      wnt = case wanted of
        Just s -> statmap
        Nothing -> Map.insert stat 0 statmap
  in over (currentStatus . statModifications) (Map.adjust (plus level) stat) .
     over (currentStatus . statModifications) (\_ -> wnt) $ pkmn

makeFlinched :: PokemonState -> PokemonState
makeFlinched = set (currentStatus . flinched) Flinched

removeFlinched = set (currentStatus . flinched) NotFlinched

makeConfused :: Int -> PokemonState -> PokemonState
makeConfused i = set (currentStatus . confused) (Confused i)

makeLeechSeeded :: PokemonState -> PokemonState
makeLeechSeeded = set (currentStatus . leechSeeded) LeechSeeded

makeYawned y = set (currentStatus . yawned) y

makeProtected = set (currentStatus . protected) Protected
removeProtected = set (currentStatus . protected) NotProtected
