{-# LANGUAGE TemplateHaskell #-}

module Domain.Match.Damage where

import qualified Domain.Attribute.Damage as D
import qualified Domain.Attribute.CriticalHit as C
import Domain.Attribute.Counterparty
import Domain.Attribute.Ability
import Domain.Attribute.HeldItem
import Domain.Attribute.ModifStat
import Domain.Attribute.Statistic as St
import Domain.Attribute.MoveExecution
import Domain.Attribute.HP
import Domain.Attribute.Category
import Domain.Attribute.TypeOf
import Domain.Match.Validation
import Domain.Match.TypeEffect
import Control.Lens

totalDamage :: St.Level -> D.Damage -> D.Damage -> D.Damage -> D.Damage
totalDamage lvl baseDam ratioDam restDam =
 restDam <> D.Damage (2 + act)
  where
    D.Damage act = (baseDam <> ratioDam <> (D.Damage $ lvl' / 50))
    lvl' = (2 * lvl'') / 5 + 2 :: Double
    lvl'' = fromIntegral lvl

data DamageRatio =
  RatioByCategory Category
  | DetailedRatio (Counterparty,ModifStat) (Counterparty,ModifStat)
  deriving (Eq,Show)

makePrisms ''DamageRatio

data ModificationSource =
  TargetWeight
  | UserHP
  | TargetHP
  | ConditionHolds Validation
  deriving (Eq,Show)

data ModifiedBaseDamage' = ModifiedBaseDamage'
  {
    _modificationBase :: Int
  , _modificationRule :: (Int -> Int)
  , _modificationSrc :: ModificationSource
  }

makeLenses ''ModifiedBaseDamage'

instance Show ModifiedBaseDamage' where
  show m = "ModifiedBasedamage' " ++ (show $ view modificationBase m) ++
    " f :: (Int -> Int) " ++ (show $ view modificationSrc m)

instance Eq ModifiedBaseDamage' where
  (ModifiedBaseDamage' b r s) == (ModifiedBaseDamage' b' r' s') =
    b == b' &&
    s == s' &&
    map r v == map r' v
    where
      v = [0..1000]

data BaseDamage =
  BaseDamage Int
  | ModifiedBaseDamage ModifiedBaseDamage'
  deriving (Eq,Show)

makePrisms ''BaseDamage

data CriticalHitCalc =
  CriticalHitCalc Int
  | ModifyCriticalHitCalc Validation (Int -> Int) CriticalHitCalc

instance Eq CriticalHitCalc where
  CriticalHitCalc a == CriticalHitCalc b = a == b
  ModifyCriticalHitCalc v f x == ModifyCriticalHitCalc v' f' x' =
    v == v' &&
    map f [-1..10] == map f' [-1..10] &&
    x == x'
  _ == _ = False

instance Show CriticalHitCalc where
  show (CriticalHitCalc v) = "CriticalHitCalc " ++ show v
  show (ModifyCriticalHitCalc v _ x) =
    "ModifyCriticalhitcalc " ++ show v ++ " f :: Int -> Int " ++ show x
  
modifyCriticalHitCalc :: CriticalHitCalc -> CriticalHitCalc
modifyCriticalHitCalc (CriticalHitCalc x) =
  ModifyCriticalHitCalc scopeLens (\s -> s + 1) $
  ModifyCriticalHitCalc shellArmor (\_ -> 0) (CriticalHitCalc x)
  where
    scopeLens = User `heldItemIs` (\it -> it == Just ScopeLens)
    shellArmor = notMoldBreaker `And` (Target `abilityIs` (\ab -> ab `elem` [ShellArmor]))
modifyCriticalHitCalc x = x

data Stab = Stab TypeOf Double deriving (Eq,Show)

data TypeAdvantage =
  TypeAdvantage TypeOf
  | ExtendedTypeAdvantage TypeOf [(TypeOf,TypeEffect)]
  | IfSuperEffective Validation (Double -> Double) TypeAdvantage
  | BerryForDefence (Maybe HeldItem -> Double -> Double) TypeAdvantage

instance Eq TypeAdvantage where
  TypeAdvantage t == TypeAdvantage t' = t == t'
  ExtendedTypeAdvantage t ts == ExtendedTypeAdvantage t' ts' =
    t == t' &&
    ts == ts'
  IfSuperEffective v f a == IfSuperEffective v' f' a' =
    v == v' &&
    a == a' &&
    map f [2,3,4] == map f' [2,3,4]
  BerryForDefence f a == BerryForDefence f' a' =
    a == a' &&
    map (uncurry f) its == map (uncurry f') its
    where
      its = do
        nms <- [0,0.25,0.5,1.00,2.00,4.00]
        its' <- allHeldItems
        return $ (Just its',nms)
  _ == _ = False

instance Show TypeAdvantage where
  show (TypeAdvantage x) = "TypeAdvantage " ++ show x
  show (ExtendedTypeAdvantage t ls) = "ExtendedTypeAdvantage " ++ show t ++ " " ++ show ls
  show (IfSuperEffective v f a) = "IfSuperEffective " ++ show v ++ "(f :: Double -> Double) (" ++ show a ++ ")"
  show (BerryForDefence f a) = "BerryForDefence (f :: Maybe HeldItem -> Double -> Double) (" ++ show a ++ ")"
    

data OtherFactors =
  LightScreenAffected
  | ReflectAffected
  | DigAmplifies OtherFactors
  | DiveAmplifies OtherFactors
  | MinimizeAmplifies OtherFactors
  | ImmunityIf Validation OtherFactors
  deriving (Eq,Show)

defaultOtherFactors :: Category -> OtherFactors
defaultOtherFactors Physical = ReflectAffected
defaultOtherFactors _ = LightScreenAffected

data StandardDamage' =
  StandardDamage'
  {
    _baseDamage :: BaseDamage
  , _damageRatio :: DamageRatio
  , _criticalHitCalc :: CriticalHitCalc
  , _stab :: Stab
  , _typeAdvantage :: TypeAdvantage
  , _otherFactors :: OtherFactors
  } deriving (Eq,Show)

makeLenses ''StandardDamage'

data MultiStrike' = MultiStrike'
  {
    _standardDamageForStrikes :: StandardDamage'
  , _minHits :: Int
  , _maxHits :: Int
  } deriving (Eq,Show)

makeLenses ''MultiStrike'

data ConstantDamage' = ConstantDamage'
  {
    _constantDamageDone :: D.Damage
  , _constantDamageTypeAdvantage :: TypeAdvantage
  } deriving (Eq,Show)

makeLenses ''ConstantDamage'

data FromPrevDamage' = FromPrevDamage'
  {
    _fromMoveExecution :: MoveExecution -> D.Damage
  , _prevDamageTypeAdvantage :: TypeAdvantage
  }

makeLenses ''FromPrevDamage'

instance Show FromPrevDamage' where
  show f = "FromPrevDamage' (f :: MoveExecution -> Int) " ++
    (show $ view prevDamageTypeAdvantage f)

newtype OHKO' = OHKO' {_ohkoTypeAdvantage :: TypeAdvantage} deriving (Eq,Show)

data Damage =
  StandardDamage StandardDamage'
  | MultiStrike MultiStrike'
  | ConstantDamage ConstantDamage'
  | FromPrevDamage FromPrevDamage'
  | OHKO OHKO'
  deriving (Show)

makePrisms ''Damage

defaultDamage :: Int -> Category -> Damage
defaultDamage basepower cat =
  StandardDamage $
  StandardDamage'
  {
    _baseDamage = BaseDamage basepower
  , _damageRatio = RatioByCategory cat
  , _criticalHitCalc = CriticalHitCalc 0
  , _stab = Stab Normal 1.5
  , _typeAdvantage = TypeAdvantage Normal
  , _otherFactors = defaultOtherFactors cat
  }

defaultMultiStrike :: Int -> Category -> Damage
defaultMultiStrike basepower cat =
  MultiStrike $
  MultiStrike'
  {
    _standardDamageForStrikes =
    (\(Just a) -> a) . preview _StandardDamage $ defaultDamage basepower cat
  , _minHits = 2
  , _maxHits = 5
  }

