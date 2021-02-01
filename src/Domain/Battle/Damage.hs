{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TemplateHaskell, UndecidableInstances#-}

module Domain.Battle.Damage where

import Domain.Attribute.Damage as D
import Domain.Attribute.Statistic
import Domain.Attribute.Screen
import Domain.Attribute.ModifStat
import Domain.Attribute.MoveExecution
import Domain.Attribute.CriticalHit
import Domain.Attribute.Category
import Domain.Attribute.HP
import Domain.Attribute.TypeOf
import Domain.Attribute.Counterparty
import Domain.Attribute.Player
import qualified Control.Lens as L
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Reader
import Domain.Entity.Battle
import Domain.Entity.Pokemon.Species as PSc
import Domain.Entity.Pokemon as P
import qualified Domain.Entity.PokemonState as PSt
import Domain.Battle.Algebra as Alg
import Domain.Battle.Logic
import Domain.Match.Damage
import Domain.Match.Stat
import Domain.Match.TypeEffect as TE
import Domain.Entity.PokemonState
import Domain.Attribute.CriticalHit
import Data.Maybe

data DamageInformation = DamageInformation
  {
    _currentStateOfBattle :: Battle
  , _lastMoveExecution :: Maybe MoveExecution
  , _hitWasCritical :: CriticalHit
  , _criticalHitShock :: Double
  , _randomMultiplierShock :: Double
  } deriving (Show)

L.makeLenses ''DamageInformation

stateOf = L.view currentStateOfBattle

instance BattleAlgebra DamageInformation where
  getUser = getUser . stateOf
  getCurrentPokemon pl = getCurrentPokemon pl . stateOf
  updateCurrentPokemon fn pl = L.over currentStateOfBattle (Alg.updateCurrentPokemon fn pl)
  getWeather = getWeather . stateOf
  getPlayer pl = getPlayer pl . stateOf
  updatePlayer fn pl = L.over currentStateOfBattle (Alg.updatePlayer fn pl)
  setWeather w i = L.over currentStateOfBattle (Alg.setWeather w i)

instance ReadPreviousStrike DamageInformation where
  askPrevStrike = L.view lastMoveExecution

getCriticalHit :: (MonadState DamageInformation m) => m CriticalHit
getCriticalHit = L.view hitWasCritical <$> get
  
class EvalDamage m where
  evalDamage :: (MonadState DamageInformation a) => m -> a D.Damage

instance EvalDamage BaseDamage where
  evalDamage (BaseDamage x) = return . D.Damage . fromIntegral $ x
  evalDamage (ModifiedBaseDamage x) = evalDamage x

instance EvalDamage ModifiedBaseDamage' where
  evalDamage (ModifiedBaseDamage' b r s) =
    case s of
      ConditionHolds v -> do
        bttle <- get
        let lg = evalLogic bttle v
        return $ case lg of
          True -> D.Damage . fromIntegral $ r b
          False -> D.Damage . fromIntegral $ b
      TargetWeight -> do
        bttle <- get
        let getWght =  L.view (pokemonIndividual . P.species . PSc.weight)
        let wght = getTargetPokemon getWght bttle
        return $ Damage $ case wght of
          Just x -> x
          Nothing -> 0

instance EvalDamage DamageRatio where
  evalDamage (DetailedRatio (cp1, ms1) (cp2, ms2)) = do
    let pl1 = case cp1 of
          User -> getUser
          Target -> getTarget
    let pl2 = case cp2 of
          User -> getUser
          Target -> getTarget
    bttle <- get
    let usr = pl1 bttle >>= flip getCurrentPokemon bttle
    let trgt = pl2 bttle >>= flip getCurrentPokemon bttle

    -- Hae varsinaiset tilastot
    -- Muuta ensin StatModif -> BaseStat, jotta getStatistic-funktio toimii
    let mstf m = case m of
          Attack' -> BaseAttack
          SAttack' -> BaseSAttack
          SDefence' -> BaseSDefence
          Defence' -> BaseDefence
    let usrStat = getStatistic (mstf ms1) <$> usr
    let trgtStat = getStatistic (mstf ms2) <$> trgt
    
    -- Hae muutokset
    let modgetter m = (\x -> case x of Just y -> y; Nothing -> 0) .
          Map.lookup m .
          L.view (currentStatus . statModifications)
    -- Hae kriittinen isku
    cr <- getCriticalHit
    let usrMod = modif4userStat ms1 cr <$> modgetter ms1 <$> usr
    let trgtMod = modif4targetStat ms2 cr <$> modgetter ms2 <$> trgt

    -- Laske ration yläpuoli
    let topOf = (*) <$> usrMod <*> (fromIntegral <$> usrStat)
    -- Ja sitten alapuoli
    let botOf = (*) <$> trgtMod <*> (fromIntegral <$> trgtStat)

    let fromMaybe = \x -> case x of
          Just y -> y
          Nothing -> 1

    let dam = Damage $ (fromMaybe topOf) / (fromMaybe botOf)
    return dam

  evalDamage (RatioByCategory Physical) =
    evalDamage (DetailedRatio (User, Attack') (Target, Defence'))
  evalDamage (RatioByCategory Special) =
    evalDamage (DetailedRatio (User, SAttack') (Target, SDefence'))
  evalDamage (RatioByCategory Status) = 
    evalDamage (DetailedRatio (User, Speed') (Target, Speed'))

instance EvalDamage TypeAdvantage where
  evalDamage (TypeAdvantage moveType) = evalDamage (ExtendedTypeAdvantage moveType [])
  evalDamage (ExtendedTypeAdvantage mvType x) = extendedEval TE.typeEffect mvType x
      where
        extendedEval f tp [] = do
          battle <- get
          let trgttp = getTargetPokemon (L.view (currentAttributes . PSt.typeOf)) battle
          let dam = D.Damage <$> TE.totalTypeEffect f tp <$> trgttp
          return $ case dam of
            Just x -> x
            Nothing -> D.Damage 0
        extendedEval f tp (x:xs) =
          let (trgtt, eff) = x
          in extendedEval (\mt tt -> if tt == trgtt then eff else f mt tt) tp xs
  evalDamage (IfSuperEffective v f a) = do
    btl <- get
    let b = evalLogic btl v
    (D.Damage d) <- evalDamage a
    return $ case (b,d >= 2) of
      (True,True) -> Damage $ f d
      _ -> Damage d
  evalDamage (BerryForDefence f a) = do
    (D.Damage d) <- evalDamage a
    btl <- get
    let item = getUserPokemon (L.view (currentAttributes . PSt.heldItem)) btl
    let effafter = case flip f d <$> item of
          Nothing -> 1
          Just x -> x
    -- Implementoi esineen pudotus
    -- TODO !!!!!!!!!!!!!!!!!!!!
    ---------------------
    return $ D.Damage $ d * effafter

instance EvalDamage OtherFactors where
  evalDamage (ImmunityIf v o) = do
    btl <- get
    let b = evalLogic btl v
    let mult = if b then 0 else 1
    dam <- evalDamage o
    return $ dam <> (Damage mult)
  evalDamage LightScreenAffected = do
    btl <- get
    cr <- getCriticalHit
    let ls = do
          plr <- getTarget btl
          st <- getPlayer plr btl
          let x = L.view (playerCondition . lightScreen) st
          return $ x
    return $ case cr of
      NotCritical -> case ls of
        Just (LightScreen x) -> Damage 0.5
        _ -> Damage 1
      _ -> Damage 1
  evalDamage ReflectAffected = do
    btl <- get
    cr <- getCriticalHit
    let ls = do
          plr <- getTarget btl
          st <- getPlayer plr btl
          let x = L.view (playerCondition . reflect) st
          return $ x
    return $ case cr of
      NotCritical -> case ls of
        Just (Reflect x) -> Damage 0.5
        _ -> Damage 1
      _ -> Damage 1
  --
  -- Implementoi loput!
  -- TODO !!!!!!!!!!!
  evalDamage (DigAmplifies x) = evalDamage x
  evalDamage (DiveAmplifies x) = evalDamage x
  evalDamage (MinimizeAmplifies x) = evalDamage x

instance EvalDamage Stab where
  evalDamage (Stab t m) = do
    btl <- get
    let usert = getUserPokemon (L.view (currentAttributes . PSt.typeOf)) btl
    let isel = elem t <$> usert
    return $ case isel of
      Just True -> Damage m
      _ -> Damage 1
     
instance EvalDamage StandardDamage' where
  evalDamage x = do
    btl <- get
    let rands = L.view randomMultiplierShock btl
    let crs = L.view criticalHitShock btl
    --- Implementoi kriittinen isku!!!
    --- TODO !!!!!!!!!!!!!
    let l = getUserPokemon (L.view (pokemonIndividual . pokemonLevel)) btl
    let l' = case l of
          Just x -> x
          Nothing -> 50
    total <- totalDamage l' <$>
      evalDamage (L.view baseDamage x) <*>
      evalDamage (L.view damageRatio x) <*>
      evalDamageRest (0.15 * rands + 0.85)
    return total
    where
      evalDamageRest rns =
        (return $ Damage rns) <<>>
        evalDamage (L.view stab x) <<>>
        evalDamage (L.view typeAdvantage x) <<>>
        evalDamage (L.view otherFactors x)
      x <<>> y = (<>) <$> x <*> y
    
instance EvalDamage OHKO' where
  evalDamage (OHKO' x) = do
    btl <- get
    let curhp = do
          trgt <- getTarget btl
          pkmn <- getCurrentPokemon trgt btl
          let curh = L.view (hpAmount.currentHp) pkmn
          return curh
    return . Damage . fromIntegral . fromMaybe 0 $ curhp

          
