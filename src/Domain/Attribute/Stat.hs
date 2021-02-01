module Domain.Attribute.Stat where

import Domain.Attribute.Nature

newtype Level = Level Int deriving (Eq, Show)
newtype MaxHP = MaxHP Int deriving (Show,Eq)
data HP = HP Int MaxHP 
newtype Attack = Attack {getAttack :: Int} deriving (Eq, Show)
newtype Defence = Defence {getDefence :: Int} deriving (Eq, Show)
newtype SAttack = SAttack {getSAttack :: Int} deriving (Eq, Show)
newtype SDefence = SDefence {getSDefence :: Int} deriving (Eq, Show)
newtype Speed = Speed {getSpeed :: Int} deriving (Eq, Show)
newtype Accuracy = Accuracy Double deriving (Eq, Show)
newtype Evasion = Evasion Double deriving (Eq, Show)
newtype Weight = Weight Double deriving (Eq, Show)
newtype EffortValue = EffortValue Int deriving (Eq, Show)
newtype IndividualValue = IndividualValue Int deriving (Eq, Show)

mkHpFromMax :: MaxHP -> HP
mkHpFromMax (MaxHP m) = mkHp m

mkHp :: Int -> HP
mkHp maxV = HP val $ (MaxHP val)
  where
    val = max 1 maxV

setHp :: Int -> HP -> HP
setHp x (HP _ z) = HP xs z
  where
    xs = min zs . max 0 $ x
    MaxHP zs = z

addHp :: Int -> HP -> HP
addHp x (HP a m) = setHp (x + a) (HP a m)

mkAttack = Attack . max 0
mkDefence = Defence . max 0
mkSAttack = SAttack . max 0
mkSDefence = SDefence . max 0
mkSpeed = Speed . max 0
mkWeight = Weight . max 0
mkIV = IndividualValue . min 31 . max 0
mkEV = EffortValue . min 255 . max 0

hpPct :: HP -> Double
hpPct (HP a (MaxHP v)) = fromRational . (/) (toRational a) $ (toRational v)

toDouble :: Int -> Double
toDouble = fromRational . toRational


instance Show HP where
  show (HP v (MaxHP a)) = "HP: " ++ hpVal ++ " / " ++ maxHpVal  ++ " (" ++ div ++ " %)"
    where
      hpVal = show v
      maxHpVal = show a
      div = take 4 . show . (*100) . hpPct $ (HP v (MaxHP a))

class HPComparison a where
  (€>)  :: a -> HP -> Bool
  (€>=) :: a -> HP -> Bool
  (€<)  :: a -> HP -> Bool
  (€<=) :: a -> HP -> Bool
  (€==) :: a -> HP -> Bool

class Stat a where
  statistic :: Level -> Nature -> EffortValue -> IndividualValue -> a -> a
  nature :: a -> Nature -> Double

class StatDenominator a where
  toDenominator :: a -> Double

class StatNominator a where
  toNominator :: a -> Double
  
instance HPComparison Int where
  (€>)  x (HP a b) = x > a
  (€>=) x (HP a b) = x >= a
  (€<)  x (HP a b) = x < a
  (€<=) x (HP a b) = x <= a
  (€==) x (HP a b) = x == a
  
instance HPComparison Double where
  (€>)  x = (>)  x . hpPct 
  (€>=) x = (>=) x . hpPct 
  (€<)  x = (<)  x . hpPct  
  (€<=) x = (<=) x . hpPct 
  (€==) x = (==) x . hpPct 
 
instance Eq HP where
  (==) (HP a b) (HP c d) = a == c
  (/=) (HP a b) (HP c d) = a /= c

instance Ord HP where
  (<)  (HP a b) (HP c d) = a < c
  (<=) (HP a b) (HP c d) = a <= c
  (>)  (HP a b) (HP c d) = a > c
  (>=) (HP a b) (HP c d) = a >= c


instance Stat MaxHP where
  nature _ _ = 1
  statistic (Level level) _ ev iv (MaxHP base) = MaxHP $ 10 + level + floorHp
    where floorHp = (level * nomin) `div` 100
          nomin   = 2 * base + ivVal + floored
          IndividualValue ivVal = iv
          floored = floor $ (toDouble evs) / 4
          EffortValue evs = ev

otherStat :: Int -> (Nature -> Double) -> Level -> Nature -> EffortValue -> IndividualValue -> Int
otherStat base natureFn (Level level) nat ev iv = floor . (* rest) $ natureFn nat
  where
    rest = toDouble . (+ 5) $ divid `div` 100
    divid = level * nomin
    nomin = 2 * base + ivs + floored
    floored = floor $ (toDouble evs) / 4
    IndividualValue ivs = iv
    EffortValue evs = ev

instance Stat Attack where
  statistic level nat ev iv (Attack value) =
    Attack $ otherStat value (nature $ Attack value) level nat ev iv 
  nature stat nat
    | nat `elem` [Lonely,Brave,Adamant,Naughty] = 1.1
    | nat `elem` [Bold,Timid,Modest,Calm] = 0.9
    | otherwise = 1

instance Stat Defence where
  statistic level nat ev iv (Defence value) =
    Defence $ otherStat value (nature $ Defence value) level nat ev iv
  nature stat nat
    | nat `elem` [Bold,Relaxed,Impish,Lax] = 1.1
    | nat `elem` [Lonely,Hasty,Mild,Gentle] = 0.9
    | otherwise = 1

instance Stat SAttack where
  statistic level nat ev iv (SAttack value) =
    SAttack $ otherStat value (nature $ SAttack value) level nat ev iv
  nature stat nat
    | nat `elem` [Modest,Mild,Quiet,Rash] = 1.1
    | nat `elem` [Adamant,Impish,Jolly,Careful] = 0.9
    | otherwise = 1

instance Stat SDefence where
  statistic level nat ev iv (SDefence value) =
    SDefence $ otherStat value (nature $ SDefence value) level nat ev iv
  nature stat nat
    | nat `elem` [Calm,Gentle,Sassy,Careful] = 1.1
    | nat `elem` [Naughty,Lax,Naive,Rash] = 0.9
    | otherwise = 1

instance Stat Speed where
  statistic level nat ev iv (Speed value) =
    Speed $ otherStat value (nature $ Speed value) level nat ev iv
  nature stat nat
    | nat `elem` [Timid,Hasty,Jolly,Naive] = 1.1
    | nat `elem` [Naughty,Lax,Naive,Rash] = 0.9
    | otherwise = 1

instance StatNominator Attack where
  toNominator (Attack x) = toDouble x

instance StatNominator SAttack where
  toNominator (SAttack x) = toDouble x

instance StatNominator Accuracy where
  toNominator (Accuracy x) = x

instance StatDenominator Defence where
  toDenominator (Defence x) = (1/) . toDouble $ x

instance StatDenominator SDefence where
  toDenominator (SDefence x) = (1/) . toDouble $ x

instance StatDenominator Evasion where
  toDenominator (Evasion x) = 1.0 / x

(€/) :: (StatNominator a, StatDenominator b) => a -> b -> Double
a €/ b = (toNominator a) * (toDenominator b)
