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
  | Overcoat
  | Overgrow
  | PoisonPoint
  | Pressure
  | PrismArmor
  | RainDish
  | Rivalry
  | RockHead
  | RoughSkin
  | SandForce
  | SandRush
  | SandVeil
  | SereneGrace
  | SheerForce
  | ShellArmor
  | Sniper
  | SnowCloak
  | SolidRock
  | Soundproof
  | Static
  | Steadfast
  | Sturdy
  | SuctionCups
  | SwiftSwim
  | Synchronize
  | Teravolt
  | ThickFat
  | Torrent
  | Turboblaze
  | VitalSpirit
  | WaterAbsorb
  | WaterBubble
  | WaterVeil
  | WhiteSmoke
  deriving (Eq, Show, Read, Enum, Ord, Bounded)

allAbilities :: [Ability]
allAbilities = enumFrom minAbility
  where
    minAbility = minBound :: Ability
