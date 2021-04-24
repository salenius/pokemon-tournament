module Domain.Attribute.Statistic where

import Domain.Attribute.Nature

data BaseStat =
  BaseHP
  | BaseAttack
  | BaseDefence
  | BaseSAttack
  | BaseSDefence
  | BaseSpeed
  deriving (Eq,Show,Ord,Enum)

type Statistic = Int
type Level = Int

nature :: BaseStat -> Nature -> Double
nature BaseAttack Lonely = 1.1
nature BaseAttack Brave = 1.1
nature BaseAttack Adamant = 1.1
nature BaseAttack Naughty = 1.1
nature BaseAttack Bold = 0.9
nature BaseAttack Timid = 0.9
nature BaseAttack Modest = 0.9
nature BaseAttack Calm = 0.9
nature BaseDefence Bold = 1.1
nature BaseDefence Relaxed = 1.1
nature BaseDefence Impish = 1.1
nature BaseDefence Lax = 1.1
nature BaseDefence Lonely = 0.9
nature BaseDefence Hasty = 0.9
nature BaseDefence Mild = 0.9
nature BaseDefence Gentle = 0.9
nature BaseSAttack Modest = 1.1
nature BaseSAttack Mild = 1.1
nature BaseSAttack Quiet = 1.1
nature BaseSAttack Rash = 1.1
nature BaseSAttack Adamant = 0.9
nature BaseSAttack Impish = 0.9
nature BaseSAttack Jolly = 0.9
nature BaseSAttack Careful = 0.9
nature BaseSDefence Calm = 1.1
nature BaseSDefence Gentle = 1.1
nature BaseSDefence Sassy = 1.1
nature BaseSDefence Careful = 1.1
nature BaseSDefence Naughty = 0.9
nature BaseSDefence Lax = 0.9
nature BaseSDefence Naive = 0.9
nature BaseSDefence Rash = 0.9
nature BaseSpeed Timid = 1.1
nature BaseSpeed Hasty = 1.1
nature BaseSpeed Jolly = 1.1
nature BaseSpeed Naive = 1.1
nature BaseSpeed Brave = 0.9
nature BaseSpeed Relaxed = 0.9
nature BaseSpeed Quiet = 0.9
nature BaseSpeed Sassy = 0.9
nature _ _ = 1


