{-# LANGUAGE TemplateHaskell #-}

module Domain.Entity.Move where

import Domain.Attribute.Choice
import Domain.Attribute.MoveExecution
import Domain.Attribute.ModifStat
import qualified Domain.Attribute.Damage as D
import Domain.Match.Damage
import Domain.Match.SideEffect
import Domain.Match.BattleEffect
import Domain.Match.MoveSucceeds
import Domain.Match.Validation
import qualified Domain.Match.Accuracy as Ac
import Domain.Attribute.TypeOf
import Domain.Attribute.Category
import Domain.Attribute.Ailment
import Domain.Attribute.Ability
import qualified Domain.Attribute.Weather as W
import Domain.Attribute.Counterparty
import Control.Lens
import Control.Lens.Prism


data Accuracy =
  Accuracy Double
  | AlwaysHits
  deriving (Eq,Show,Ord)

defaultPowerPoints :: Int
defaultPowerPoints = 15

defaultChoiceProbability :: ChoiceRule Validation
defaultChoiceProbability = ChoiceRule NormalProbability

defaultSuccessionType :: SuccessType
defaultSuccessionType = NormallyExecuted

defaultHitProbability :: Ac.HitProbability
defaultHitProbability = Ac.BasicAccuracy $ Ac.Accuracy 1.00

data MoveSpecies = MoveSpecies
  {
    _nameOfMove :: String
  , _typeOf :: TypeOf
  , _category :: Category
  , _powerPoints :: Int
  , _priority :: Int
  , _accuracy :: Accuracy
  } deriving (Eq,Show)

makeLenses ''MoveSpecies

makeDefaultSpecies :: Category -> TypeOf -> MoveSpecies
makeDefaultSpecies cat tp =
  MoveSpecies "" tp cat defaultPowerPoints 0 $ Accuracy 1.0
  

data MoveNecessities = MoveNecessities
  {
    _species :: MoveSpecies
  , _choiceProbability :: ChoiceRule Validation
  , _executionSuccess :: SuccessType 
  , _hitProbability :: Ac.HitProbability
  , _sideEffect :: SideEffect
  } deriving (Show)

makeLenses ''MoveNecessities

makeDefaultNecessities :: Category -> TypeOf -> SideEffect -> MoveNecessities
makeDefaultNecessities cat tp eff =
  MoveNecessities (makeDefaultSpecies cat tp) defaultChoiceProbability defaultSuccessionType defaultHitProbability eff

data StrikeMove' = StrikeMove'
  {
  _basepower :: Maybe Int
  , _makesContact :: Bool
  , _damage :: Damage
  , _strikeNecessities :: MoveNecessities
  } deriving (Show)

makeLenses ''StrikeMove'

newtype StatusMove' = StatusMove' {_statusNecessities :: MoveNecessities}

makeLenses ''StatusMove'

data CallbackMove' m = CallbackMove'
  {
    _availableMoves :: [m]
  , _callbackNecessities :: MoveNecessities
  } deriving (Show)

makeLenses ''CallbackMove'

data Move =
  StrikeMove StrikeMove'
  | StatusMove StatusMove'
  | CallbackMove (CallbackMove' Move)

makePrisms ''Move

necessities :: Lens' Move MoveNecessities
necessities = lens getter setter
  where
    getter m@(StrikeMove mv) = fromMaybe $ preview (_StrikeMove . strikeNecessities) m
    getter m@(StatusMove mv) = fromMaybe $ preview (_StatusMove . statusNecessities) m
    getter m@(CallbackMove mv) = fromMaybe $ preview (_CallbackMove . callbackNecessities) m
    setter m@(StrikeMove mv) n = set (_StrikeMove . strikeNecessities) n m
    setter m@(StatusMove mv) n = set (_StatusMove . statusNecessities) n m
    setter m@(CallbackMove mv) n = set (_CallbackMove . callbackNecessities) n m
    fromMaybe (Just a) = a

instance Show Move where
  show mv =
    preview' (sp . nameOfMove) mv ++ " <" ++ typ mv ++ ", " ++ cat mv ++ ">"
    where
      preview' a b = fromMaybe . preview a $ b
      sp = necessities . species
      cat m = show . fromMaybe $ preview (necessities . species . category) m
      typ m = show . fromMaybe $ preview (necessities . species . typeOf) m
      fromMaybe (Just a) = a

instance Eq Move where
  mv == mv' =
    preview l mv == preview l mv' ||
    preview l' mv == preview l' mv' ||
    preview l'' mv == preview l'' mv' 
    where
      l = _StrikeMove . strikeNecessities . species
      l' = _StatusMove . statusNecessities . species
      l'' = _CallbackMove . callbackNecessities . species

instance Ord Move where
  StrikeMove m <= StatusMove m' = False
  StrikeMove m <= StrikeMove m' =
    (preview basepower $ m) <= (preview basepower $ m')
  _ <= _ = False

makeStatusMove :: TypeOf -> SideEffect -> Move
makeStatusMove tp eff =
  StatusMove . StatusMove' $ makeDefaultNecessities Status tp eff
    

makeStrikeMove :: Category -> Int -> TypeOf -> SideEffect -> Move
makeStrikeMove cat basep tp eff =
  StrikeMove $ StrikeMove'
  {
    _basepower = Just basep
  , _makesContact = if cat == Physical then True else False
  , _damage =
    set (_StandardDamage . stab) (Stab tp 1.5) $
    set (_StandardDamage . typeAdvantage) (TypeAdvantage tp) $
    defaultDamage basep cat 
  , _strikeNecessities = makeDefaultNecessities cat tp eff
  
  }

makeCallbackMove :: TypeOf -> [Move] -> Move
makeCallbackMove tp mvs =
  CallbackMove $ CallbackMove'
  {
    _availableMoves = mvs
  , _callbackNecessities = makeDefaultNecessities Status tp mempty
  }

physicalMove = makeStrikeMove Physical
specialMove = makeStrikeMove Special

-- noContact x = x { _makesContact = False}
noContact :: Move -> Move
noContact  = set (_StrikeMove . makesContact) False

assignName :: String -> Move -> Move
assignName nm = set (necessities . species . nameOfMove) nm

assignPP :: Int -> Move -> Move
assignPP pp = set (necessities . species . powerPoints) pp

assignAccuracy :: Double -> Move -> Move
assignAccuracy prob =
  set (necessities . species . accuracy) (Accuracy prob) .
  set (necessities . hitProbability) (Ac.BasicAccuracy $ Ac.Accuracy prob)

assignChoiceProbability :: ChoiceRule Validation -> Move -> Move
assignChoiceProbability cr = set (necessities . choiceProbability) cr

alwaysHits :: Move -> Move
alwaysHits = set (necessities . species . accuracy) AlwaysHits

assignPriority :: Int -> Move -> Move
assignPriority prio = set (necessities . species . priority) prio

effects :: SideEffect -> Move -> Move
effects es = set (necessities . sideEffect) es


modifyBaseDamage :: Int -> ModificationSource -> (Int -> Int) -> Move -> Move
modifyBaseDamage b src fn = 
  set (_StrikeMove . damage . _StandardDamage . baseDamage . _ModifiedBaseDamage) x
  where
    x = ModifiedBaseDamage' b fn src

modifyOtherFactors :: (OtherFactors -> OtherFactors) -> Move -> Move
modifyOtherFactors factor m = 
  over (_StrikeMove . damage . _StandardDamage . otherFactors) (\o -> factor o) m 

modifyCriticalHit :: (CriticalHitCalc -> CriticalHitCalc) -> Move -> Move
modifyCriticalHit crhit = 
  over (_StrikeMove . damage . _StandardDamage . criticalHitCalc) (\o -> crhit o)

targetAbilityImmunity :: Ability -> Validation
targetAbilityImmunity ab = notMoldBreaker `Or` (abilityIs Target $ (==) ab)

bulletproof = targetAbilityImmunity Bulletproof
soundproof = targetAbilityImmunity Soundproof

