module Domain.Battle.Choice where

import Domain.Entity.Battle
import Domain.Entity.Move
import Domain.Battle.TypeEffect

data ProbabilityWeight =
  Infinite
  | ProbabilityWeight Int
  deriving (Eq,Show)

instance Num ProbabilityWeight where
  ProbabilityWeight a + ProbabilityWeight b = ProbabilityWeight $ a + b
  Infinite + _ = Infinite
  _ + Infinite = Infinite
  ProbabilityWeight 0 * _ = ProbabilityWeight 0
  _ * ProbabilityWeight 0 = ProbabilityWeight 0
  ProbabilityWeight a * ProbabilityWeight b = ProbabilityWeight $ a * b
  Infinite * _ = Infinite
  _ * Infinite = Infinite
  negate Infinite = Infinite
  negate _ = ProbabilityWeight 0
  abs = id
  signum (ProbabilityWeight 0) = ProbabilityWeight 0
  signum x = ProbabilityWeight 1

class ChooseMove m where
  giveProbability :: m -> ChoicePhase -> Int

data ProbabilityWeighting =
  PropabilityWeighting
  {
    weightBasepower :: Int -> Int
  , weightTypeAdvantage :: Double -> Int
  }

