{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Attribute.TypeOf where

import Types.Pokemon
import Types.BuiltIn
import GHC.Generics

class IsTypeOf o where
  asNormal :: o -> Maybe NormalType
  asFighting :: o -> Maybe FightingType
  asFlying :: o -> Maybe FlyingType
  asWater :: o -> Maybe WaterType
  asFire :: o -> Maybe FireType
  asGrass :: o -> Maybe GrassType
  asElectric :: o -> Maybe ElectricType
  asBug :: o -> Maybe BugType
  asPoison :: o -> Maybe PoisonType
  asRock :: o -> Maybe RockType
  asGround :: o -> Maybe GroundType
  asSteel :: o -> Maybe SteelType
  asIce :: o -> Maybe IceType
  asPsychic :: o -> Maybe PsychicType
  asDark :: o -> Maybe DarkType
  asGhost :: o -> Maybe GhostType
  asDragon :: o -> Maybe DragonType
  asFairy :: o -> Maybe FairyType


data NormalType = NormalType deriving (Eq,Show,Read,Ord,Generic)
data FightingType = FightingType deriving (Eq,Show,Read,Ord,Generic)
data FlyingType = FlyingType deriving (Eq,Show,Read,Ord,Generic)
data WaterType = WaterType deriving (Eq,Show,Read,Ord,Generic)
data FireType = FireType deriving (Eq,Show,Read,Ord,Generic)
data GrassType = GrassType deriving (Eq,Show,Read,Ord,Generic)
data ElectricType = ElectricType deriving (Eq,Show,Read,Ord,Generic)
data BugType = BugType deriving (Eq,Show,Read,Ord,Generic)
data PoisonType = PoisonType deriving (Eq,Show,Read,Ord,Generic)
data RockType = RockType deriving (Eq,Show,Read,Ord,Generic)
data GroundType = GroundType deriving (Eq,Show,Read,Ord,Generic)
data SteelType = SteelType deriving (Eq,Show,Read,Ord,Generic)
data IceType = IceType deriving (Eq,Show,Read,Ord,Generic)
data PsychicType = PsychicType deriving (Eq,Show,Read,Ord,Generic)
data DarkType = DarkType deriving (Eq,Show,Read,Ord,Generic)
data GhostType = GhostType deriving (Eq,Show,Read,Ord,Generic)
data DragonType = DragonType deriving (Eq,Show,Read,Ord,Generic)
data FairyType = FairyType deriving (Eq,Show,Read,Ord,Generic)

instance IsTypeOf PokemonType where
  asNormal pt =  if Normal `elem` pt then Just NormalType else Nothing
  asFighting pt =  if Fighting `elem` pt then Just FightingType else Nothing
  asFlying pt =  if Flying `elem` pt then Just FlyingType else Nothing
  asWater pt =  if Water `elem` pt then Just WaterType else Nothing
  asFire pt =  if Fire `elem` pt then Just FireType else Nothing
  asGrass pt =  if Grass `elem` pt then Just GrassType else Nothing
  asElectric pt =  if Electric `elem` pt then Just ElectricType else Nothing
  asBug pt =  if Bug `elem` pt then Just BugType else Nothing
  asPoison pt =  if Poison `elem` pt then Just PoisonType else Nothing
  asRock pt =  if Rock `elem` pt then Just RockType else Nothing
  asGround pt =  if Ground `elem` pt then Just GroundType else Nothing
  asSteel pt =  if Steel `elem` pt then Just SteelType else Nothing
  asIce pt =  if Ice `elem` pt then Just IceType else Nothing
  asPsychic pt =  if Psychic `elem` pt then Just PsychicType else Nothing
  asDark pt =  if Dark `elem` pt then Just DarkType else Nothing
  asGhost pt =  if Ghost `elem` pt then Just GhostType else Nothing
  asDragon pt =  if Dragon `elem` pt then Just DragonType else Nothing
  asFairy pt =  if Fairy `elem` pt then Just FairyType else Nothing
