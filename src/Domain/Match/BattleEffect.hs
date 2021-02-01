module Domain.Match.BattleEffect where

data BattleEffect a v =
  Apply a
  | Chain (BattleEffect a v) (BattleEffect a v)
  | Spread (BattleEffect a v) (BattleEffect a v)
  | ForRounds Int (BattleEffect a v)
  | WithProbability Double (BattleEffect a v)
  | ValidateEffect v (BattleEffect a v)
  deriving (Eq,Show)

instance Semigroup (BattleEffect a v) where
  a <> b = Spread a b

instance Monoid a => Monoid (BattleEffect a v) where
  mempty = Apply mempty

foldLogic :: (Monoid a, Eq a, Eq v) => (v -> Bool) -> BattleEffect a v -> BattleEffect a v
foldLogic _ (Apply a) = Apply a
foldLogic f (ForRounds d x)
  | foldLogic f x == mempty = mempty
  | otherwise = ForRounds d $ foldLogic f x
foldLogic f (WithProbability i x)
  | foldLogic f x == mempty = mempty
  | otherwise = WithProbability i $ foldLogic f x
foldLogic f (ValidateEffect v x)
  | f v == True = foldLogic f x
  | otherwise = mempty
foldLogic f (Spread a b)
  | foldLogic f a == mempty && foldLogic f b == mempty = mempty
  | otherwise = Spread (foldLogic f a) (foldLogic f b)
foldLogic f (Chain a b)
  | foldLogic f a == mempty = mempty
  | otherwise = Chain (foldLogic f a) (foldLogic f b)

