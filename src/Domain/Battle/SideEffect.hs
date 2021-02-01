{-# LANGUAGE TemplateHaskell, FlexibleContexts, MultiParamTypeClasses #-}

module Domain.Battle.SideEffect where

import Domain.Match.SideEffect as SE
import Domain.Match.Validation as V
import Domain.Match.BattleEffect as BE
import Domain.Entity.Battle as B
import Domain.Entity.PokemonState as PS
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Domain.Attribute.Damage as D
import Domain.Attribute.MoveExecution
import Domain.Attribute.HP
import Domain.Battle.Damage
import Domain.Battle.Logic
import Domain.Attribute.Ailment
import Domain.Attribute.Counterparty
import Domain.Battle.Algebra as Alg
import Data.Maybe

class MonadState a b => EffectMonad a b where
  resetRandomDouble :: b ()

data BattleEffectFolded a =
  ApplyFolded a
  | ChainFolded (BattleEffectFolded a) (BattleEffectFolded a)
  | SpreadFolded (BattleEffectFolded a) (BattleEffectFolded a)
  | ForRoundsFolded Int (BattleEffectFolded a)
  deriving (Eq,Show)

instance Semigroup (BattleEffectFolded a) where
  a <> b = SpreadFolded a b

instance Monoid a => Monoid (BattleEffectFolded a) where
  mempty = ApplyFolded mempty

data EffectInformation = EffectInformation
  {
    _battleAtEffect :: Battle
  , _damageMade :: Maybe D.Damage
  , _prevStrikeInfo :: Maybe MoveExecution
  , _genRandomDouble :: Double
  }

makeLenses ''EffectInformation

currentBattleState = view battleAtEffect

instance BattleAlgebra EffectInformation where
  getUser = getUser . currentBattleState
  getCurrentPokemon pl = getCurrentPokemon pl . currentBattleState
  updateCurrentPokemon fn pl = over battleAtEffect (Alg.updateCurrentPokemon fn pl)
  getWeather = getWeather . currentBattleState
  getPlayer pl = getPlayer pl . currentBattleState
  updatePlayer fn pl = over battleAtEffect (Alg.updatePlayer fn pl)
  setWeather w i = over battleAtEffect (Alg.setWeather w i)

instance ReadPreviousStrike EffectInformation where
  askPrevStrike = view prevStrikeInfo


damageInfo2effectInfo :: DamageInformation -> EffectInformation
damageInfo2effectInfo damInfo =
  EffectInformation
  {
    _battleAtEffect = view currentStateOfBattle damInfo
  , _damageMade = Nothing
  , _prevStrikeInfo = view lastMoveExecution damInfo
  , _genRandomDouble = 0
  }

damageIntoEffectInfo :: D.Damage -> EffectInformation -> EffectInformation
damageIntoEffectInfo dam = set damageMade (Just dam)

--- Interpreters

foldEffects :: (MonadReader EffectInformation m, EffectMonad EffectInformation m)
  => SE.SideEffect
  -> m (BattleEffectFolded SE.SideEffectType)
foldEffects (BE.Apply a) = return $ ApplyFolded a
foldEffects (BE.ValidateEffect v a) = do
  bttl <- ask
  let b = evalLogic bttl v
  if b then foldEffects a else return mempty
foldEffects (BE.Chain x y) = do
  x' <- foldEffects x
  y' <- foldEffects y
  let b1 = x' == mempty 
  let b2 = y' == mempty
  return $ case b1 of
    True -> mempty
    False -> case b2 of
      True -> x'
      False -> ChainFolded x' y'
foldEffects (BE.Spread x y) = do
  x' <- foldEffects x
  y' <- foldEffects y
  let b1 = x' == mempty 
  let b2 = y' == mempty
  return $ case (b1,b2) of
    (True,True) -> mempty
    (True,False) -> y'
    (False,True) -> x'
    (False,False) -> SpreadFolded x' y'
foldEffects (BE.ForRounds i a) = do
  a' <- foldEffects a
  let b = a' == mempty
  return $ if b then mempty else ForRoundsFolded i a'
foldEffects (BE.WithProbability d a) = do
  resetRandomDouble
  a' <- foldEffects a
  btl <- ask
  let randD = view genRandomDouble btl
  return $ if (randD < d) then a' else mempty

evalEffect :: BattleEffectFolded SE.SideEffectType -> EffectInformation -> EffectInformation
evalEffect (ChainFolded a b)  = evalEffect b. evalEffect a
evalEffect (SpreadFolded a b) = evalEffect b. evalEffect a
evalEffect (ApplyFolded a)    = evalEff a
  where
    modifPokemon User                      = modifyUserPokemon
    modifPokemon Target                    = modifyTargetPokemon
    modifPlayer  User                      = Alg.modifyUser
    modifPlayer  Target                    = Alg.modifyTarget
    evalEff SE.DoNothing                   = id
    evalEff (SE.CauseAilment cp ai)        =
      modifPokemon cp (case ai of
                           Burned    -> setBurned
                           Paralyzed -> setParalyzed
                           Poisoned  -> setPoisoned
                           Frozen    -> setFrozen 3
                           Sleep     -> setSleep 3
                           Healthy   -> setAilmentForTurns Healthy 0)
    evalEff (SE.ThawOut cp)                = evalEff (SE.CauseAilment cp Healthy)
    evalEff (SE.AddHP cp (SE.Amount a))    = modifPokemon cp (addHpAmount a)
    evalEff (SE.AddHP cp (SE.PercentOf p)) = modifPokemon cp (addHpPercent p)
    evalEff (SE.AddHP _ SE.Equalize)       = h
      where f c battle =
              let ownhp = do
                    ohp        <- view currentHp <$> getter c
                    usr        <- view currentHp <$> getter User
                    trt        <- view currentHp <$> getter Target
                    let total2 =  floor $ fromIntegral usr / fromIntegral trt
                    return     $  total2 - ohp

                  getter c = do
                    x <- case c of
                      User -> getUser battle
                      Target -> getTarget battle
                    p <- getCurrentPokemon x battle
                    return $ view hpAmount p

              in ownhp

            g cp amt battle = evalEff (SE.AddHP cp (SE.Amount (fromMaybe 0 $ amt))) battle
            h battle        = g User (f User battle) . g Target (f Target battle) $ battle

    evalEff (SE.DropItem cp)                    = modifPokemon cp PS.dropItem
    evalEff (SE.IncreaseStat cp stat lvl)       =
      modifPokemon cp (addStatModification stat lvl)
    evalEff (SE.DropStat cp stat lvl)           =
      modifPokemon cp (addStatModification stat (-lvl))
    evalEff SE.CauseFlinch                      = modifyTargetPokemon makeFlinched
    evalEff (SE.CauseConfusion cp)              = modifPokemon cp (makeConfused 3)
    evalEff (SE.DrainDamage pct)                = f pct
      where f pct b = fromMaybe b $ do
              (D.Damage damAmt) <- view damageMade b
              return             $ modifyUserPokemon (addHpAmount (floor $ pct * damAmt)) b
    evalEff (SE.CauseRecoil pct)                = f pct
      where f pct b = fromMaybe b $ do
              (D.Damage damAmt) <- view damageMade b
              return             $ modifyUserPokemon (addHpAmount (floor . negate $ pct * damAmt)) b
    evalEff SE.CauseLeechSeeded                 = modifyTargetPokemon makeLeechSeeded
    evalEff (SE.PutScreen SE.LightScreen cp)    = modifPlayer cp (B.putLightScreen 5)
    evalEff (SE.PutScreen SE.Reflect cp)        = modifPlayer cp (B.putReflect 5)
    evalEff (SE.RemoveScreen SE.LightScreen cp) = modifPlayer cp B.removeLightScreen
    evalEff (SE.RemoveScreen SE.Reflect cp)     = modifPlayer cp B.removeReflect 
    evalEff SE.ProtectUser                      = modifyUserPokemon makeProtected
    evalEff SE.YawnTarget                       = modifyTargetPokemon $
      (\x -> case view (currentStatus . yawned) x of
               NotYawned -> makeYawned YawnedFirstRound x
               _ -> x)
    evalEff (SE.SwitchPokemon cp)               = modifPlayer cp (B.switchPokemon (const True))
    evalEff (SE.ChangeWeather w)                = setWeather w 5

evalEffect (ForRoundsFolded i (ApplyFolded x)) = case x of
  SE.PutScreen SE.LightScreen cp -> modifPlayer  cp (B.putLightScreen i)
  SE.PutScreen SE.Reflect cp     -> modifPlayer  cp (B.putReflect i)
  SE.CauseAilment cp Frozen      -> modifPokemon cp (setFrozen i)
  SE.CauseAilment cp Sleep       -> modifPokemon cp (setSleep i)
  SE.CauseConfusion cp           -> modifPokemon cp (makeConfused i)
  SE.ChangeWeather w             -> setWeather    w i
  _                              -> evalEffect    $ ApplyFolded x
  where
    modifPokemon User = modifyUserPokemon
    modifPokemon Target = modifyTargetPokemon
    modifPlayer User = Alg.modifyUser
    modifPlayer Target = Alg.modifyTarget
evalEffect (ForRoundsFolded _ x) = evalEffect x
