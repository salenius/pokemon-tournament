{-# LANGUAGE TemplateHaskell #-}

module Domain.Move.DamageMove (
  DamageMoveBuiltIn(..),
  DirectDamageMoveBuiltIn(..),
  _DamageMoveBuiltIn,
  _DirectDamageMoveBuiltIn
                              ) where

import Domain.Move.BuiltIn
import Control.Applicative
import Control.Lens

data DamageMoveBuiltIn =
  Struggle'
  | Acrobatics'
  | AirSlash'
  | AquaJet'
  | AuraSphere'
  | Avalanche'
  | Blizzard'
  | BodySlam'
  | BraveBird'
  | BugBuzz'
  | BulletPunch'
  | BrickBreak'
  | CloseCombat'
  | Crunch'
  | DarkPulse'
  | DracoMeteor'
  | DragonClaw'
  | DragonPulse'
  | EarthPower'
  | Earthquake'
  | EnergyBall'
  | ExtremeSpeed'
  | FakeOut'
  | FireBlast'
  | FirePunch'
  | Flamethrower'
  | FlareBlitz'
  | FlashCannon'
  | FocusBlast'
  | GigaDrain'
  | GrassKnot'
  | HammerArm'
  | HeadSmash'
  | HeatWave'
  | HydroPump'
  | IceBeam'
  | IceFang'
  | IcePunch'
  | IceShard'
  | IcyWind'
  | IronTail'
  | LeafStorm'
  | MachPunch'
  | MuddyWater'
  | Outrage'
  | Payback'
  | Psychic'
  | QuickAttack'
  | RockBlast'
  | RockSlide'
  | Scald'
  | SeedBomb'
  | ShadowBall'
  | SignalBeam'
  | SludgeBomb'
  | SolarBeam'
  | StoneEdge'
  | SuckerPunch'
  | Superpower'
  | Surf'
  | UTurn'
  | VoltTackle'
  | Waterfall'
  | WaterPulse'
  | WaterSpout'
  | WildCharge'
  | WoodHammer'
  | XScissor'
  | ZenHeadbutt'
  deriving (Eq,Show,Read,Ord,Enum)

makePrisms ''DamageMoveBuiltIn

data DirectDamageMoveBuiltIn =
  MetalBurst'
  | SheerCold'
  deriving (Eq,Show,Read,Ord,Enum)

makePrisms ''DirectDamageMoveBuiltIn

_DamageMoveBuiltIn :: Prism' BuiltInMove DamageMoveBuiltIn
_DamageMoveBuiltIn = prism fromDamageMove (\mv -> case toDamageMove mv of
                                              Nothing -> Left mv
                                              Just m -> Right m)

_DirectDamageMoveBuiltIn :: Prism' BuiltInMove DirectDamageMoveBuiltIn
_DirectDamageMoveBuiltIn = prism fromDirectDamageMove (\mv -> case toDirectDamageMove mv of
                                              Nothing -> Left mv
                                              Just m -> Right m)



--- Helper functions

fromDamageMove :: DamageMoveBuiltIn -> BuiltInMove
fromDamageMove Struggle' = Struggle
fromDamageMove Acrobatics' = Acrobatics
fromDamageMove AirSlash' = AirSlash
fromDamageMove AquaJet' = AquaJet
fromDamageMove AuraSphere' = AuraSphere
fromDamageMove Avalanche' = Avalanche
fromDamageMove Blizzard' = Blizzard
fromDamageMove BodySlam' = BodySlam
fromDamageMove BraveBird' = BraveBird
fromDamageMove BugBuzz' = BugBuzz
fromDamageMove BulletPunch' = BulletPunch
fromDamageMove BrickBreak' = BrickBreak
fromDamageMove CloseCombat' = CloseCombat
fromDamageMove Crunch' = Crunch
fromDamageMove DarkPulse' = DarkPulse
fromDamageMove DracoMeteor' = DracoMeteor
fromDamageMove DragonClaw' = DragonClaw
fromDamageMove DragonPulse' = DragonPulse
fromDamageMove EarthPower' = EarthPower
fromDamageMove Earthquake' = Earthquake
fromDamageMove EnergyBall' = EnergyBall
fromDamageMove ExtremeSpeed' = ExtremeSpeed
fromDamageMove FakeOut' = FakeOut
fromDamageMove FireBlast' = FireBlast
fromDamageMove FirePunch' = FirePunch
fromDamageMove Flamethrower' = Flamethrower
fromDamageMove FlareBlitz' = FlareBlitz
fromDamageMove FlashCannon' = FlashCannon
fromDamageMove FocusBlast' = FocusBlast
fromDamageMove GigaDrain' = GigaDrain
fromDamageMove GrassKnot' = GrassKnot
fromDamageMove HammerArm' = HammerArm
fromDamageMove HeadSmash' = HeadSmash
fromDamageMove HeatWave' = HeatWave
fromDamageMove HydroPump' = HydroPump
fromDamageMove IceBeam' = IceBeam
fromDamageMove IceFang' = IceFang
fromDamageMove IcePunch' = IcePunch
fromDamageMove IceShard' = IceShard
fromDamageMove IcyWind' = IcyWind
fromDamageMove IronTail' = IronTail
fromDamageMove LeafStorm' = LeafStorm
fromDamageMove MachPunch' = MachPunch
fromDamageMove MuddyWater' = MuddyWater
fromDamageMove Outrage' = Outrage
fromDamageMove Payback' = Payback
fromDamageMove Psychic' = Psychic
fromDamageMove QuickAttack' = QuickAttack
fromDamageMove RockBlast' = RockBlast
fromDamageMove RockSlide' = RockSlide
fromDamageMove Scald' = Scald
fromDamageMove SeedBomb' = SeedBomb
fromDamageMove ShadowBall' = ShadowBall
fromDamageMove SignalBeam' = SignalBeam
fromDamageMove SludgeBomb' = SludgeBomb
fromDamageMove SolarBeam' = SolarBeam
fromDamageMove StoneEdge' = StoneEdge
fromDamageMove SuckerPunch' = SuckerPunch
fromDamageMove Superpower' = Superpower
fromDamageMove Surf' = Surf
fromDamageMove UTurn' = UTurn
fromDamageMove VoltTackle' = VoltTackle
fromDamageMove Waterfall' = Waterfall
fromDamageMove WaterPulse' = WaterPulse
fromDamageMove WaterSpout' = WaterSpout
fromDamageMove WildCharge' = WildCharge
fromDamageMove WoodHammer' = WoodHammer
fromDamageMove XScissor' = XScissor
fromDamageMove ZenHeadbutt' = ZenHeadbutt


toDamageMove :: BuiltInMove -> Maybe DamageMoveBuiltIn
toDamageMove Struggle = Just Struggle'
toDamageMove Acrobatics = Just Acrobatics'
toDamageMove AirSlash = Just AirSlash'
toDamageMove AquaJet = Just AquaJet'
toDamageMove AuraSphere = Just AuraSphere'
toDamageMove Avalanche = Just Avalanche'
toDamageMove Blizzard = Just Blizzard'
toDamageMove BodySlam = Just BodySlam'
toDamageMove BraveBird = Just BraveBird'
toDamageMove BugBuzz = Just BugBuzz'
toDamageMove BulletPunch = Just BulletPunch'
toDamageMove BrickBreak = Just BrickBreak'
toDamageMove CloseCombat = Just CloseCombat'
toDamageMove Crunch = Just Crunch'
toDamageMove DarkPulse = Just DarkPulse'
toDamageMove DracoMeteor = Just DracoMeteor'
toDamageMove DragonClaw = Just DragonClaw'
toDamageMove DragonPulse = Just DragonPulse'
toDamageMove EarthPower = Just EarthPower'
toDamageMove Earthquake = Just Earthquake'
toDamageMove EnergyBall = Just EnergyBall'
toDamageMove ExtremeSpeed = Just ExtremeSpeed'
toDamageMove FakeOut = Just FakeOut'
toDamageMove FireBlast = Just FireBlast'
toDamageMove FirePunch = Just FirePunch'
toDamageMove Flamethrower = Just Flamethrower'
toDamageMove FlareBlitz = Just FlareBlitz'
toDamageMove FlashCannon = Just FlashCannon'
toDamageMove FocusBlast = Just FocusBlast'
toDamageMove GigaDrain = Just GigaDrain'
toDamageMove HammerArm = Just HammerArm'
toDamageMove HeadSmash = Just HeadSmash'
toDamageMove HeatWave = Just HeatWave'
toDamageMove HydroPump = Just HydroPump'
toDamageMove IceBeam = Just IceBeam'
toDamageMove IceFang = Just IceFang'
toDamageMove IcePunch = Just IcePunch'
toDamageMove IceShard = Just IceShard'
toDamageMove IcyWind = Just IcyWind'
toDamageMove IronTail = Just IronTail'
toDamageMove LeafStorm = Just LeafStorm'
toDamageMove MachPunch = Just MachPunch'
toDamageMove MuddyWater = Just MuddyWater'
toDamageMove Outrage = Just Outrage'
toDamageMove Payback = Just Payback'
toDamageMove Psychic = Just Psychic'
toDamageMove QuickAttack = Just QuickAttack'
toDamageMove RockBlast = Just RockBlast'
toDamageMove RockSlide = Just RockSlide'
toDamageMove Scald = Just Scald'
toDamageMove SeedBomb = Just SeedBomb'
toDamageMove ShadowBall = Just ShadowBall'
toDamageMove SignalBeam = Just SignalBeam'
toDamageMove SludgeBomb = Just SludgeBomb'
toDamageMove SolarBeam = Just SolarBeam'
toDamageMove StoneEdge = Just StoneEdge'
toDamageMove SuckerPunch = Just SuckerPunch'
toDamageMove Superpower = Just Superpower'
toDamageMove Surf = Just Surf'
toDamageMove UTurn = Just UTurn'
toDamageMove VoltTackle = Just VoltTackle'
toDamageMove Waterfall = Just Waterfall'
toDamageMove WaterPulse = Just WaterPulse'
toDamageMove WaterSpout = Just WaterSpout'
toDamageMove WildCharge = Just WildCharge'
toDamageMove WoodHammer = Just WoodHammer'
toDamageMove XScissor = Just XScissor'
toDamageMove ZenHeadbutt = Just ZenHeadbutt'
toDamageMove _ = Nothing

fromDirectDamageMove :: DirectDamageMoveBuiltIn -> BuiltInMove
fromDirectDamageMove SheerCold' = SheerCold
fromDirectDamageMove MetalBurst' = MetalBurst

toDirectDamageMove :: BuiltInMove -> Maybe DirectDamageMoveBuiltIn
toDirectDamageMove SheerCold = Just SheerCold'
toDirectDamageMove MetalBurst = Just MetalBurst'
toDirectDamageMove _ = Nothing
