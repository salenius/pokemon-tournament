module Domain.Attribute.ModifStat (
  ModifStat(..),
  ModifStatLevel(..),
  modifStatLevel,
  addModifStatLevel,
  modifStatLevelToInt,
  modifStatLevelToMultiplier
  
                                  ) where

data ModifStat =
  Attack'
  | Defence'
  | SAttack'
  | SDefence'
  | Speed'
  | Accuracy'
  | Evasion'
  deriving (Eq,Show,Ord)

data ModifStatLevel =
  Minus6
 | Minus5
 | Minus4
 | Minus3
 | Minus2
 | Minus1
 | LevelZero
 | Plus1
 | Plus2
 | Plus3
 | Plus4
 | Plus5
 | Plus6
 deriving (Eq,Show,Ord)

modifStatLevel :: Int -> Maybe ModifStatLevel
modifStatLevel x
  | x > 6 = Nothing
  | x < -6 = Nothing
  | otherwise = case x of
      -6 -> Just Minus6
      -5 -> Just Minus5
      -4 -> Just Minus4
      -3 -> Just Minus3
      -2 -> Just Minus2
      -1 -> Just Minus1
      0 -> Just LevelZero
      1 -> Just Plus1
      2 -> Just Plus2
      3 -> Just Plus3
      4 -> Just Plus4
      5 -> Just Plus5
      6 -> Just Plus6
      
addOneModifStatLevel :: ModifStatLevel -> ModifStatLevel
addOneModifStatLevel x = case x of
  Minus6 -> Minus5
  Minus5 -> Minus4
  Minus3 -> Minus2
  Minus2 -> Minus1
  Minus1 -> LevelZero
  LevelZero -> Plus1
  Plus1 -> Plus2
  Plus2 -> Plus3
  Plus3 -> Plus4
  Plus4 -> Plus5
  Plus5 -> Plus6
  _ -> x

subtractOneModifStatLevel :: ModifStatLevel -> ModifStatLevel
subtractOneModifStatLevel x = case x of
  Minus5 -> Minus6
  Minus4 -> Minus5
  Minus2 -> Minus3
  Minus1 -> Minus2
  LevelZero -> Minus1
  Plus1 -> LevelZero
  Plus2 -> Plus1
  Plus3 -> Plus2
  Plus4 -> Plus3
  Plus5 -> Plus4
  Plus6 -> Plus5
  _ -> x


addModifStatLevel :: Int -> ModifStatLevel -> ModifStatLevel
addModifStatLevel y x
  | y > 0 = addModifStatLevel (y - 1) $ addOneModifStatLevel x
  | y < 0 = addModifStatLevel (y + 1) $ subtractOneModifStatLevel x
  | y == 0 = x

modifStatLevelToInt :: ModifStatLevel -> Int
modifStatLevelToInt Minus6 = -6
modifStatLevelToInt Minus5 = -5
modifStatLevelToInt Minus4 = -4
modifStatLevelToInt Minus3 = -3
modifStatLevelToInt Minus2 = -2
modifStatLevelToInt Minus1 = -1
modifStatLevelToInt LevelZero = 0
modifStatLevelToInt Plus6 = 6
modifStatLevelToInt Plus5 = 5
modifStatLevelToInt Plus4 = 4
modifStatLevelToInt Plus3 = 3
modifStatLevelToInt Plus2 = 2
modifStatLevelToInt Plus1 = 1


--

modifStatLevelToMultiplier :: ModifStat -> Int -> Double
modifStatLevelToMultiplier Accuracy' lvl = levelToMultiplier lvl (9,9)
modifStatLevelToMultiplier Evasion' lvl = levelToMultiplier lvl (9,9)
modifStatLevelToMultiplier stat lvl = levelToMultiplier lvl (8,8)

levelToMultiplier :: Int -> (Int,Int) -> Double
levelToMultiplier s (x,y)
  | s == 0 = fromIntegral x / fromIntegral y
  | s > 0 = levelToMultiplier (s - 1) (x, y - 1)
  | s < 0 = levelToMultiplier (s + 1) (x - 1, y)
