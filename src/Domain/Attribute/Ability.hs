{-# LANGUAGE LambdaCase #-}

module Domain.Attribute.Ability where

data Ability =
  NoAbility
  | Aerilate
  | BattleArmor
  | BigPecks
  | Blaze
  | Bulletproof
  | Chlorophyll
  | ClearBody
  | CloudNine
  | Comatose
  | Defeatist
  | Filter
  | FlameBody
  | FlashFire
  | FlowerVeil
  | FullMetalBody
  | Guts
  | Hustle
  | HyperCutter
  | IceBody
  | Illuminate
  | Immunity
  | InnerFocus
  | Insomnia
  | Intimidate
  | KeenEye
  | LeafGuard
  | Levitate
  | LiquidVoice
  | Limber
  | MagicBounce
  | MagicGuard
  | MagmaArmor
  | MarvelScale
  | Merciless
  | MoldBreaker
  | Moxie
  | NaturalCure
  | Neuroforce
  | NoGuard
  | Normalize
  | Oblivious
  | Overcoat
  | Overgrow
  | PoisonPoint
  | Pressure
  | PrismArmor
  | RainDish
  | Regenerator
  | Rivalry
  | RockHead
  | RoughSkin
  | SandForce
  | SandRush
  | SandVeil
  | SapSipper
  | SereneGrace
  | SheerForce
  | ShellArmor
  | Sniper
  | SnowCloak
  | SolidRock
  | Soundproof
  | Static
  | Steadfast
  | StormDrain
  | Sturdy
  | SuctionCups
  | SwiftSwim
  | Synchronize
  | Teravolt
  | ThickFat
  | Torrent
  | Turboblaze
  | VitalSpirit
  | VoltAbsorb
  | WaterAbsorb
  | WaterBubble
  | WaterVeil
  | WhiteSmoke
  deriving (Eq, Show, Read, Enum, Ord, Bounded)

data IsMoldBreaker =
  IsMoldBreaker
  | IsTeravolt
  | IsTurboblaze
  deriving (Eq,Show)

isMoldBreaker :: Ability -> Maybe IsMoldBreaker
isMoldBreaker = \case
  MoldBreaker -> Just IsMoldBreaker
  Teravolt -> Just IsTeravolt
  Turboblaze -> Just IsTurboblaze
  _ -> Nothing

allAbilities :: [Ability]
allAbilities = enumFrom minAbility
  where
    minAbility = minBound :: Ability
