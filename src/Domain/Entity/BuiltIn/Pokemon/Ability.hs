{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}


module Domain.Entity.BuiltIn.Pokemon.Ability where

import Domain.Attribute.Ability
import Control.Lens

data Lapras'sAbility =
  Lapras'sWaterAbsorb
  | Lapras'sShellArmor
  deriving (Eq,Show,Read)

data Snorlax'sAbility =
  Snorlax'sImmunity
  | Snorlax'sThickFat
  deriving (Eq,Show,Read)

data Aerodactyl'sAbility =
  Aerodactyl'sRockHead
  | Aerodactyl'sPressure
  deriving (Eq,Show,Read)

data Machamp'sAbility =
  Machamp'sGuts
  | Machamp'sNoGuard
  deriving (Eq,Show,Read)

data Alakazam'sAbility =
  Alakazam'sSynchronize
  | Alakazam'sInnerFocus
  deriving (Eq,Show,Read)

data Arcanine'sAbility =
  Arcanine'sIntimidate
  | Arcanine'sFlashFire
  deriving (Eq,Show,Read)

data Kingdra'sAbility =
  Kingdra'sSwiftSwim
  | Kingdra'sSniper
  deriving (Eq,Show,Read)

data Haxorus'sAbility =
  Haxorus'sRivalry
  | Haxorus'sMoldBreaker
  deriving (Eq,Show,Read)

data Aggron'sAbility =
  Aggron'sSturdy
  | Aggron'sRockHead
  deriving (Eq,Show,Read)

data Excadrill'sAbility =
  Excadrill'sSandRush
  | Excadrill'sSandForce
  deriving (Eq,Show,Read)

data Walrein'sAbility =
  Walrein'sThickFat
  | Walrein'sIceBody
  deriving (Eq,Show,Read)


data Ludicolo'sAbility =
  Ludicolo'sSwiftSwim
  | Ludicolo'sRainDish
  deriving (Eq,Show,Read)


data Starmie'sAbility =
  Starmie'sIlluminate
  | Starmie'sNaturalCure
  deriving (Eq,Show,Read)

data Roserade'sAbility =
  Roserade'sNaturalCure
  | Roserade'sPoisonPoint
  deriving (Eq,Show,Read)


data Togekiss'sAbility =
  Togekiss'sHustle
  | Togekiss'sSereneGrace
  deriving (Eq,Show,Read)


data Lucario'sAbility =
  Lucario'sSteadfast
  | Lucario'sInnerFocus
  deriving (Eq,Show,Read)


data Conkeldurr'sAbility =
  Conkeldurr'sGuts
  | Conkeldurr'sSheerForce
  deriving (Eq,Show,Read)

data Reuniclus'sAbility =
  Reuniclus'sOvercoat
  | Reuniclus'sMagicGuard
  deriving (Eq,Show,Read)


data Krookodile'sAbility =
  Krookodile'sIntimidate
  | Krookodile'sMoxie
  deriving (Eq,Show,Read)

data Chandelure'sAbility =
  Chandelure'sFlashFire
  | Chandelure'sFlameBody
  deriving (Eq,Show,Read)


data Braviary'sAbility =
  Braviary'sKeenEye
  | Braviary'sSheerForce
  deriving (Eq,Show,Read)

--

instance Semigroup Lapras'sAbility where
  (<>) = flip const 

instance Semigroup Snorlax'sAbility where
  (<>) = flip const 

instance Semigroup Aerodactyl'sAbility where
  (<>) = flip const 

instance Semigroup Machamp'sAbility where
  (<>) = flip const 

instance Semigroup Alakazam'sAbility where
  (<>) = flip const 

instance Semigroup Arcanine'sAbility where
  (<>) = flip const 

instance Semigroup Kingdra'sAbility where
  (<>) = flip const 

instance Semigroup Haxorus'sAbility where
  (<>) = flip const 

instance Semigroup Aggron'sAbility where
  (<>) = flip const 

instance Semigroup Excadrill'sAbility where
  (<>) = flip const 

instance Semigroup Walrein'sAbility where
  (<>) = flip const 

instance Semigroup Ludicolo'sAbility where
  (<>) = flip const 

instance Semigroup Starmie'sAbility where
  (<>) = flip const 


instance Semigroup Roserade'sAbility where
  (<>) = flip const 


instance Semigroup Togekiss'sAbility where
  (<>) = flip const 

instance Semigroup Lucario'sAbility where
  (<>) = flip const 

instance Semigroup Conkeldurr'sAbility where
  (<>) = flip const 

instance Semigroup Reuniclus'sAbility where
  (<>) = flip const 

instance Semigroup Krookodile'sAbility where
  (<>) = flip const 

instance Semigroup Chandelure'sAbility where
  (<>) = flip const 

instance Semigroup Braviary'sAbility where
  (<>) = flip const 



instance Monoid Lapras'sAbility where
  mempty = Lapras'sWaterAbsorb

instance Monoid Snorlax'sAbility where
  mempty = Snorlax'sImmunity

instance Monoid Aerodactyl'sAbility where
  mempty = Aerodactyl'sRockHead

instance Monoid Machamp'sAbility where
  mempty = Machamp'sGuts

instance Monoid Alakazam'sAbility where
  mempty = Alakazam'sSynchronize

instance Monoid Arcanine'sAbility where
  mempty = Arcanine'sIntimidate

instance Monoid Kingdra'sAbility where
  mempty = Kingdra'sSwiftSwim

instance Monoid Haxorus'sAbility where
  mempty = Haxorus'sRivalry

instance Monoid Aggron'sAbility where
  mempty = Aggron'sSturdy

instance Monoid Excadrill'sAbility where
  mempty = Excadrill'sSandRush

instance Monoid Walrein'sAbility where
  mempty = Walrein'sThickFat

instance Monoid Ludicolo'sAbility where
  mempty = Ludicolo'sSwiftSwim

instance Monoid Starmie'sAbility where
  mempty = Starmie'sIlluminate

instance Monoid Roserade'sAbility where
  mempty = Roserade'sNaturalCure


instance Monoid Togekiss'sAbility where
  mempty = Togekiss'sHustle


instance Monoid Lucario'sAbility where
  mempty = Lucario'sSteadfast


instance Monoid Conkeldurr'sAbility where
  mempty = Conkeldurr'sGuts

instance Monoid Reuniclus'sAbility where
  mempty = Reuniclus'sOvercoat

instance Monoid Krookodile'sAbility where
  mempty = Krookodile'sIntimidate

instance Monoid Chandelure'sAbility where
  mempty = Chandelure'sFlashFire


instance Monoid Braviary'sAbility where
  mempty = Braviary'sKeenEye



---

_Lapras'sAbility :: Prism' Ability Lapras'sAbility
_Lapras'sAbility =
  mkprism2 (WaterAbsorb, Lapras'sWaterAbsorb) (ShellArmor, Lapras'sShellArmor)

_Snorlax'sAbility :: Prism' Ability Snorlax'sAbility
_Snorlax'sAbility = mkprism2 (Immunity, Snorlax'sImmunity) (ThickFat, Snorlax'sThickFat)

_Aerodactyl'sAbility :: Prism' Ability Aerodactyl'sAbility
_Aerodactyl'sAbility =
  mkprism2 (RockHead, Aerodactyl'sRockHead) (Pressure, Aerodactyl'sPressure)

_Machamp'sAbility :: Prism' Ability Machamp'sAbility
_Machamp'sAbility =
  mkprism2 (Guts, Machamp'sGuts) (NoGuard, Machamp'sNoGuard)

_Alakazam'sAbility :: Prism' Ability Alakazam'sAbility
_Alakazam'sAbility =
  mkprism2 (Synchronize, Alakazam'sSynchronize) (InnerFocus, Alakazam'sInnerFocus)

_Arcanine'sAbility :: Prism' Ability Arcanine'sAbility
_Arcanine'sAbility =
  mkprism2 (Intimidate, Arcanine'sIntimidate) (FlashFire, Arcanine'sFlashFire)

_Kingdra'sAbility :: Prism' Ability Kingdra'sAbility
_Kingdra'sAbility = mkprism2 (SwiftSwim, Kingdra'sSwiftSwim) (Sniper, Kingdra'sSniper)

_Haxorus'sAbility :: Prism' Ability Haxorus'sAbility
_Haxorus'sAbility = mkprism2 (Rivalry, Haxorus'sRivalry) (MoldBreaker, Haxorus'sMoldBreaker)

_Aggron'sAbility :: Prism' Ability Aggron'sAbility
_Aggron'sAbility =
  mkprism2 (Sturdy, Aggron'sSturdy) (RockHead, Aggron'sRockHead)

_Excadrill'sAbility :: Prism' Ability Excadrill'sAbility
_Excadrill'sAbility =
  mkprism2 (SandRush, Excadrill'sSandRush) (SandForce, Excadrill'sSandForce)

_Walrein'sAbility :: Prism' Ability Walrein'sAbility
_Walrein'sAbility =
  mkprism2 (ThickFat, Walrein'sThickFat) (IceBody, Walrein'sIceBody)

_Ludicolo'sAbility :: Prism' Ability Ludicolo'sAbility
_Ludicolo'sAbility =
  mkprism2 (SwiftSwim, Ludicolo'sSwiftSwim) (RainDish, Ludicolo'sRainDish)

_Starmie'sAbility :: Prism' Ability Starmie'sAbility
_Starmie'sAbility =
  mkprism2 (Illuminate, Starmie'sIlluminate) (NaturalCure, Starmie'sNaturalCure)

_Roserade'sAbility :: Prism' Ability Roserade'sAbility
_Roserade'sAbility =
  mkprism2 (NaturalCure, Roserade'sNaturalCure) (PoisonPoint, Roserade'sPoisonPoint)

_Togekiss'sAbility :: Prism' Ability Togekiss'sAbility
_Togekiss'sAbility = mkprism2 (Hustle, Togekiss'sHustle) (SereneGrace, Togekiss'sSereneGrace)

_Lucario'sAbility :: Prism' Ability Lucario'sAbility
_Lucario'sAbility =
  mkprism2 (Steadfast, Lucario'sSteadfast) (InnerFocus, Lucario'sInnerFocus)

_Conkeldurr'sAbility :: Prism' Ability Conkeldurr'sAbility
_Conkeldurr'sAbility =
  mkprism2 (Guts, Conkeldurr'sGuts) (SheerForce, Conkeldurr'sSheerForce)

_Reuniclus'sAbility :: Prism' Ability Reuniclus'sAbility
_Reuniclus'sAbility =
  mkprism2 (Overcoat, Reuniclus'sOvercoat) (MagicGuard, Reuniclus'sMagicGuard)

_Krookodile'sAbility :: Prism' Ability Krookodile'sAbility
_Krookodile'sAbility =
  mkprism2 (Intimidate, Krookodile'sIntimidate) (Moxie, Krookodile'sMoxie)

_Chandelure'sAbility :: Prism' Ability Chandelure'sAbility
_Chandelure'sAbility =
  mkprism2 (FlashFire, Chandelure'sFlashFire) (FlameBody, Chandelure'sFlameBody)

_Braviary'sAbility :: Prism' Ability Braviary'sAbility
_Braviary'sAbility = mkprism2 (KeenEye, Braviary'sKeenEye) (SheerForce, Braviary'sSheerForce)

mkprism2 :: Eq a => (Ability, a) -> (Ability, a) -> Prism' Ability a
mkprism2 (abil1, othrab1) (abil2, othrab2) = prism f g
  where
    f ((==) othrab1 -> True) = abil1
    f ((==) othrab2 -> True) = abil2
    g = \case
      ((==) abil1 -> True) -> Right othrab1
      ((==) abil2 -> True) -> Right othrab2
      x -> Left x

