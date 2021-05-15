{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Attribute.TypeOf where

import Types.Pokemon
import Types.BuiltIn
import GHC.Generics
import Control.Lens

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

_NormalType :: Prism' TypeOf NormalType
_NormalType = prism' (\_ -> Normal) (\x -> case x of Normal -> Just NormalType; _ -> Nothing)

_FightingType :: Prism' TypeOf FightingType
_FightingType = prism' (\_ -> Fighting) (\x  -> case x of Fighting -> Just FightingType; _ -> Nothing)

_FlyingType :: Prism' TypeOf FlyingType
_FlyingType = prism' (\_ -> Flying) (\x  -> case x of Flying -> Just FlyingType; _ -> Nothing)

_WaterType :: Prism' TypeOf WaterType
_WaterType = prism' (\_ -> Water) (\x  -> case x of Water -> Just WaterType; _ -> Nothing)

_FireType :: Prism' TypeOf FireType
_FireType = prism' (\_ -> Fire) (\x  -> case x of Fire -> Just FireType; _ -> Nothing)

_GrassType :: Prism' TypeOf GrassType
_GrassType = prism' (\_ -> Grass) (\x  -> case x of Grass -> Just GrassType; _ -> Nothing)

_ElectricType :: Prism' TypeOf ElectricType
_ElectricType = prism' (\_ -> Electric) (\x  -> case x of Electric -> Just ElectricType; _ -> Nothing)

_BugType :: Prism' TypeOf BugType
_BugType = prism' (\_ -> Bug) (\x  -> case x of Bug -> Just BugType; _ -> Nothing)

_PoisonType :: Prism' TypeOf PoisonType
_PoisonType = prism' (\_ -> Poison) (\x  -> case x of Poison -> Just PoisonType; _ -> Nothing)

_RockType :: Prism' TypeOf RockType
_RockType = prism' (\_ -> Rock) (\x  -> case x of Rock -> Just RockType; _ -> Nothing)

_GroundType :: Prism' TypeOf GroundType
_GroundType = prism' (\_ -> Ground) (\x  -> case x of Ground -> Just GroundType; _ -> Nothing)

_SteelType :: Prism' TypeOf SteelType
_SteelType = prism' (\_ -> Steel) (\x  -> case x of Steel -> Just SteelType; _ -> Nothing)

_IceType :: Prism' TypeOf IceType
_IceType = prism' (\_ -> Ice) (\x  -> case x of Ice -> Just IceType; _ -> Nothing)

_PsychicType :: Prism' TypeOf PsychicType
_PsychicType = prism' (\_ -> Psychic) (\x  -> case x of Psychic -> Just PsychicType; _ -> Nothing)

_DarkType :: Prism' TypeOf DarkType
_DarkType = prism' (\_ -> Dark) (\x  -> case x of Dark -> Just DarkType; _ -> Nothing)

_GhostType :: Prism' TypeOf GhostType
_GhostType = prism' (\_ -> Ghost) (\x  -> case x of Ghost -> Just GhostType; _ -> Nothing)

_DragonType :: Prism' TypeOf DragonType
_DragonType = prism' (\_ -> Dragon) (\x  -> case x of Dragon -> Just DragonType; _ -> Nothing)

_FairyType :: Prism' TypeOf FairyType
_FairyType = prism' (\_ -> Fairy) (\x  -> case x of Fairy -> Just FairyType; _ -> Nothing)

