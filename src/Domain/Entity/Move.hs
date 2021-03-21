{-# LANGUAGE TemplateHaskell #-}

module Domain.Entity.Move where

import Domain.Attribute.Choice
import Domain.Attribute.MoveExecution
import Domain.Attribute.ModifStat
import qualified Domain.Attribute.Damage as D
import Domain.Match.SideEffect hiding (LightScreen, Reflect)
import Domain.Match.Validation
import qualified Domain.Match.Accuracy as Ac
import Domain.Attribute.TypeOf hiding (Psychic)
import qualified Domain.Attribute.TypeOf as T
import Domain.Attribute.Category
import Domain.Attribute.Ailment
import Domain.Attribute.Ability
import qualified Domain.Attribute.Weather as W
import Domain.Attribute.Counterparty
import Control.Lens

data StatusMove =
  AcidArmor
  | LeechSeed
  | LightScreen
  | PainSplit
  | QuiverDance
  | Protect
  | RainDance
  | Reflect
  | Rest
  | Sandstorm
  | SleepPowder
  | SleepTalk
  | SwordsDance
  | Toxic
  | WillOWisp
  | Yawn
  deriving (Eq,Show,Read,Ord,Enum)

makeClassyPrisms ''StatusMove

data DirectDamageMove =
  MetalBurst
  | SheerCold
  deriving (Eq,Show,Read,Ord,Enum)

makeClassyPrisms ''DirectDamageMove

data DamageMove =
  Struggle
  | Acrobatics
  | AirSlash
  | AquaJet
  | AuraSphere
  | Avalanche
  | Blizzard
  | BodySlam
  | BraveBird
  | BugBuzz
  | BulletPunch
  | BrickBreak
  | CloseCombat
  | Crunch
  | DarkPulse
  | DracoMeteor
  | DragonClaw
  | DragonPulse
  | EarthPower
  | Earthquake
  | EnergyBall
  | ExtremeSpeed
  | FakeOut
  | FireBlast
  | FirePunch
  | Flamethrower
  | FlareBlitz
  | FlashCannon
  | FocusBlast
  | GigaDrain
  | GrassKnot
  | HammerArm
  | HeadSmash
  | HeatWave
  | HydroPump
  | IceBeam
  | IceFang
  | IcePunch
  | IceShard
  | IcyWind
  | IronTail
  | LeafStorm
  | MachPunch
  | MuddyWater
  | Outrage
  | Payback
  | Psychic
  | QuickAttack
  | RockBlast
  | RockSlide
  | Scald
  | SeedBomb
  | ShadowBall
  | SignalBeam
  | SludgeBomb
  | SolarBeam
  | StoneEdge
  | SuckerPunch
  | Superpower
  | Surf
  | UTurn
  | VoltTackle
  | Waterfall
  | WaterPulse
  | WaterSpout
  | WildCharge
  | WoodHammer
  | XScissor
  | ZenHeadbutt
  deriving (Eq,Show,Read,Ord,Enum)

makeClassyPrisms ''DamageMove

data BuiltInMove =
  DamageMove' DamageMove
  | DirectDamageMove' DirectDamageMove
  | StatusMove' StatusMove
  deriving (Eq,Show,Read,Ord)

makeClassyPrisms ''BuiltInMove


data SuperMove =
  OtherMove
  | BuiltIn BuiltInMove
  deriving (Eq,Show)

makeClassyPrisms ''SuperMove

instance AsBuiltInMove SuperMove where
  _BuiltInMove = _BuiltIn

instance AsDamageMove SuperMove where
  _DamageMove = _DamageMove'


-- 

--

instance AsDamageMove BuiltInMove where
  _DamageMove = _DamageMove'

instance AsStatusMove BuiltInMove where
  _StatusMove = _StatusMove'

instance AsDirectDamageMove BuiltInMove where
  _DirectDamageMove = _DirectDamageMove'

--

category' :: DamageMove -> Category'
category' Acrobatics = Physical'
category' AquaJet = Physical'
category' Avalanche = Physical'
category' BodySlam = Physical'
category' BraveBird = Physical'
category' BulletPunch = Physical'
category' BrickBreak = Physical'
category' CloseCombat = Physical'
category' Crunch = Physical'
category' DragonClaw = Physical'
category' Earthquake = Physical'
category' ExtremeSpeed = Physical'
category' FakeOut = Physical'
category' FirePunch = Physical'
category' FlareBlitz = Physical'
category' HammerArm = Physical'
category' HeadSmash = Physical'
category' IceFang = Physical'
category' IcePunch = Physical'
category' IceShard = Physical'
category' IronTail = Physical'
category' MachPunch = Physical'
category' Outrage = Physical'
category' Payback = Physical'
category' QuickAttack = Physical'
category' RockBlast = Physical'
category' RockSlide = Physical'
category' SeedBomb = Physical'
category' StoneEdge = Physical'
category' Struggle = Physical'
category' SuckerPunch = Physical'
category' Superpower = Physical'
category' UTurn = Physical'
category' VoltTackle = Physical'
category' Waterfall = Physical'
category' WildCharge = Physical'
category' WoodHammer = Physical'
category' XScissor = Physical'
category' ZenHeadbutt = Physical'
category' _ = Special'

category'' :: DirectDamageMove -> Category'
category'' MetalBurst = Physical'
category'' SheerCold = Special'

makesContact' :: DamageMove -> Bool
makesContact' Earthquake = False
makesContact' GrassKnot = True
makesContact' RockBlast = False
makesContact' StoneEdge = False
makesContact' x
  | category x == Physical = True
  | otherwise = False

makesContact'' :: DirectDamageMove -> Bool
makesContact'' MetalBurst = False
makesContact'' SheerCold = False

instance TypeableMove StatusMove where
  typeOfMove LeechSeed = Grass
  typeOfMove LightScreen = T.Psychic
  typeOfMove PainSplit = Normal
  typeOfMove QuiverDance = Bug
  typeOfMove Protect = Normal
  typeOfMove RainDance = Water
  typeOfMove Reflect = T.Psychic
  typeOfMove Rest = T.Psychic
  typeOfMove Sandstorm = Rock
  typeOfMove SleepPowder = Grass
  typeOfMove SleepTalk = Normal
  typeOfMove SwordsDance = Normal
  typeOfMove Toxic = Poison
  typeOfMove WillOWisp = Fire
  typeOfMove Yawn = Normal

instance TypeableMove DamageMove where
  typeOfMove Acrobatics = Flying
  typeOfMove AquaJet = Water
  typeOfMove Avalanche = Ice
  typeOfMove AuraSphere = Fighting
  typeOfMove Blizzard = Ice
  typeOfMove BodySlam = Normal
  typeOfMove BraveBird = Flying
  typeOfMove BulletPunch = Steel
  typeOfMove BrickBreak = Fighting
  typeOfMove CloseCombat = Fighting
  typeOfMove Crunch = Dark
  typeOfMove DracoMeteor = Dragon 
  typeOfMove EnergyBall = Grass
  typeOfMove ExtremeSpeed = Normal
  typeOfMove FakeOut = Normal
  typeOfMove FlashCannon = Steel
  typeOfMove FocusBlast = Fighting
  typeOfMove GigaDrain = Grass
  typeOfMove HammerArm = Fighting
  typeOfMove HeadSmash = Rock
  typeOfMove HeatWave = Fire
  typeOfMove HydroPump = Water
  typeOfMove IronTail = Steel
  typeOfMove MachPunch = Fighting
  typeOfMove MuddyWater = Water
  typeOfMove Outrage = Dragon
  typeOfMove Payback = Dark
  typeOfMove QuickAttack = Normal
  typeOfMove Scald = Water
  typeOfMove SeedBomb = Grass
  typeOfMove SignalBeam = Bug
  typeOfMove SludgeBomb = Poison
  typeOfMove SolarBeam = Grass
  typeOfMove Struggle = Normal
  typeOfMove SuckerPunch = Dark
  typeOfMove Superpower = Fighting
  typeOfMove Surf = Water
  typeOfMove UTurn = Bug
  typeOfMove WildCharge = Electric
  typeOfMove WoodHammer = Grass
  typeOfMove XScissor = Bug
  typeOfMove ZenHeadbutt = T.Psychic
  typeOfMove x
    | take' 5 == "Water" = Water
    | take' 4 == "Rain" = Water
    | take' 4 == "Fire" = Fire
    | take' 5 == "Flame" = Fire
    | take' 5 == "Flare" = Fire
    | take' 6 == "Thunder" = Electric
    | take' 4 == "Volt" = Electric
    | take' 5 == "Grass" = Grass
    | take' 4 == "Leaf" = Grass
    | take' 3 == "Ice" = Ice
    | take' 3 == "Icy" = Ice
    | take' 5 == "Earth" = Ground
    | take' 4 == "Rock" = Rock
    | take' 5 == "Stone" = Rock
    | take' 6 == "Poison" = Poison
    | take' 6 == "Shadow" = Ghost
    | take' 3 == "Bug" = Bug
    | take' 6 == "Dragon" = Dragon
    | take' 4 == "Dark" = Dark
    | take' 5 == "Metal" = Steel
    | take' 3 == "Air" = Flying
    | take' 7 == "Psychic" = T.Psychic
    where
      take' n = take n . show $ x

instance TypeableMove DirectDamageMove where
  typeOfMove MetalBurst = Steel
  typeOfMove SheerCold = Ice

instance TypeableMove BuiltInMove where
  typeOfMove (DamageMove' mv) = typeOfMove mv
  typeOfMove (StatusMove' mv) = typeOfMove mv
  typeOfMove (DirectDamageMove' mv) = typeOfMove mv

basepower :: DamageMove -> Maybe Int
basepower Acrobatics = Just 55
basepower AquaJet = Just 40
basepower BrickBreak = Just 75
basepower DracoMeteor = Just 130
basepower FakeOut = Just 40
basepower FireBlast = Just 110
basepower GrassKnot = Nothing
basepower IronTail = Just 100
basepower LeafStorm = Just 130
basepower ShadowBall = Just 80
basepower Struggle = Just 50
basepower UTurn = Just 70
basepower VoltTackle = Just 120
basepower WaterSpout = Just 150

-- Apumäärittely

data Category' = Special' | Physical' deriving (Eq,Show,Read,Ord,Enum)

class CategorizableMove m where
  category :: m -> Category

class WrappableMove m where
  wrapMove :: m -> BuiltInMove

class TypeableMove m where
  typeOfMove :: m -> TypeOf

instance CategorizableMove StatusMove where
  category _ = Status

instance CategorizableMove DamageMove where
  category mv = case category' mv of
    Special'  -> Special
    Physical' -> Physical

instance CategorizableMove DirectDamageMove where
  category mv = case category'' mv of
    Special'  -> Special
    Physical' -> Physical


instance WrappableMove DamageMove where
  wrapMove = DamageMove'

instance WrappableMove StatusMove where
  wrapMove = StatusMove'

instance WrappableMove DirectDamageMove where
  wrapMove = DirectDamageMove'

allAttackingMoves :: [DamageMove]
allAttackingMoves = enumFrom Acrobatics

allStatusMoves :: [StatusMove]
allStatusMoves = enumFrom AcidArmor

allDirectDamageMoves :: [DirectDamageMove]
allDirectDamageMoves = enumFrom MetalBurst

allMoves :: [BuiltInMove]
allMoves = (wrapMove <$> allStatusMoves) ++ (wrapMove <$> allAttackingMoves) ++ (wrapMove <$> allDirectDamageMoves)
