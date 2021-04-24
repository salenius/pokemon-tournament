{-# LANGUAGE TemplateHaskell #-}

module Domain.Entity.BuiltIn.Move.Attacking where

import Control.Lens

data Move =
  Physical PhysicalMove
  | Special SpecialMove
  | DirectDamage DirectDamageMove
  deriving (Eq,Show,Read,Ord)

data PhysicalMove =
  Struggle
  | Acrobatics
  | AquaJet
  | Avalanche
  | BodySlam
  | BraveBird
  | BulletPunch
  | BrickBreak
  | CloseCombat
  | Crunch
  | DragonClaw
  | Earthquake
  | ExtremeSpeed
  | FakeOut
  | FirePunch
  | FlareBlitz
  | HammerArm
  | HeadSmash
  | IceFang
  | IcePunch
  | IceShard
  | IronTail
  | MachPunch
  | Outrage
  | Payback
  | QuickAttack
  | RockBlast
  | RockSlide
  | SeedBomb
  | StoneEdge
  | SuckerPunch
  | Superpower
  | UTurn
  | VoltTackle
  | Waterfall
  | WildCharge
  | WoodHammer
  | XScissor
  | ZenHeadbutt
  deriving (Eq,Show,Read,Ord,Enum)

data SpecialMove =
  AirSlash
  | AuraSphere
  | Blizzard
  | BugBuzz
  | DarkPulse
  | DracoMeteor
  | DragonPulse
  | EarthPower
  | EnergyBall
  | FireBlast
  | Flamethrower
  | FlashCannon
  | FocusBlast
  | GigaDrain
  | GrassKnot
  | HeatWave
  | HydroPump
  | IceBeam
  | IcyWind
  | LeafStorm
  | MuddyWater
  | Psychic
  | Scald
  | ShadowBall
  | SignalBeam
  | SludgeBomb
  | SolarBeam
  | Surf
  | Thunderbolt
  | WaterPulse
  | WaterSpout
  deriving (Eq,Show,Read,Ord,Enum)

data DirectDamageMove =
  MetalBurst
  | SheerCold
  deriving (Eq,Show,Read,Ord,Enum)

makeClassyPrisms ''Move
makeClassyPrisms ''SpecialMove
makeClassyPrisms ''PhysicalMove
makeClassyPrisms ''DirectDamageMove

instance AsPhysicalMove Move where
  _PhysicalMove = _Physical

instance AsSpecialMove Move where
  _SpecialMove = _Special

instance AsDirectDamageMove Move where
  _DirectDamageMove = _DirectDamage
