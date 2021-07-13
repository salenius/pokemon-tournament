{-# LANGUAGE FlexibleInstances #-}

module Domain.Entity.GenV.Ability where

data ChampionsTournamentAbility =
  Static
  | Overgrow
  | Blaze
  | Torrent
  | Immunity
  | ThickFat
  | WaterAbsorb
  | ShellArmor
  | RockHead
  | Pressure
  | Chlorophyll
  | Intimidate
  | Synchronize
  | InnerFocus
  | FlashFire
  | Guts
  | NoGuard
  | SwiftSwim
  | Sniper
  | Rivalry
  | MoldBreaker
  | Competitive
  | Levitate
  | ClearBody
  | Sturdy
  | SandRush
  | SandForce
  | Defeatist
  | SuctionCups
  | BattleArmor
  | MarvelScale
  | RoughSkin
  | IceBody
  | RainDish
  | Illuminate
  | NaturalCure
  | SandVeil
  | PoisonPoint
  | Hustle
  | SereneGrace
  | Steadfast
  | SnowCloak
  | FlameBody
  | SheerForce
  | Overcoat
  | MagicGuard
  | Moxie
  | KeenEye
  deriving (Eq,Show,Ord,Enum)

