module Domain.Entity.Move.Species where

import Domain.Attribute.Category
import Domain.Attribute.TypeOf

class (Show p) => MoveSpecies p
  -- typeOf :: p -> TypeOf
  -- category :: p -> Category
  -- category _ = Status
  -- pps :: p -> Int
  -- accuracy :: p -> Maybe Double
  -- accuracy _ = Just 1.0
  -- priority :: p -> Int
  -- priority _ = 0
  -- basepower :: p -> Maybe Int
  -- basepower _ = Nothing
  -- makesContact :: p -> Bool
  -- makesContact p
    -- | category p == Physical = True
    -- | otherwise = False

data Move = NonAttackingMove
  {
    typeOf :: TypeOf
  , category :: Category
  , pps :: Int
  , accuracy :: Maybe Double
  , priority :: Int
  } | AttackingMove
  {
    typeOf :: TypeOf
  , category :: Category
  , pps :: Int
  , accuracy :: Maybe Double
  , priority :: Int
  , basepower :: Int
  , makesContact :: Bool
  } deriving (Eq,Show)

statusMove :: Move
statusMove = NonAttackingMove
  {
    typeOf = Normal
  , category = Status
  , pps = 15
  , accuracy = Just 1.0
  , priority = 0
  }

specialMove :: Move
specialMove = AttackingMove
  {
    typeOf = Normal
  , category = Special
  , pps = 15
  , accuracy = Just 1.0
  , priority = 0
  , basepower = 90
  , makesContact = False
  }

physicalMove :: Move
physicalMove = AttackingMove
  {
    typeOf = Normal
  , category = Special
  , pps = 15
  , accuracy = Just 1.0
  , priority = 0
  , basepower = 90
  , makesContact = True
  }

airSlash = specialMove
  {
    typeOf = Flying
  , pps = 15
  , basepower = 75
  , accuracy = Just 0.95
  }

data AirSlash = AirSlash deriving (Show)

data Blizzard = Blizzard deriving (Show)

data BodySlam = BodySlam deriving (Show)

data BrickBreak = BrickBreak deriving (Show)

data Crunch = Crunch deriving (Show)

data DragonPulse = DragonPulse deriving (Show)

data Earthquake = Earthquake deriving (Show)

data FakeOut = FakeOut deriving (Show)

data FireBlast = FireBlast deriving (Show)

data FocusBlast = FocusBlast deriving (Show)

data HydroPump = HydroPump deriving (Show)

data IceBeam = IceBeam deriving (Show)

data IceShard = IceShard deriving (Show)

data IronTail = IronTail deriving (Show)

data LeafStorm = LeafStorm deriving (Show)

data SeedBomb = SeedBomb deriving (Show)

data SleepPowder = SleepPowder deriving (Show)

data SludgeBomb = SludgeBomb deriving (Show)

data Thunderbolt = Thunderbolt deriving (Show)
  
data VoltTackle = VoltTackle deriving (Show)

data WaterSpout = WaterSpout deriving (Show)

-- instance MoveSpecies AirSlash where
  -- typeOf _ = Flying
  -- category _ = Special
  -- pps _ = 15
  -- basepower _ = Just 75
  -- accuracy _ = Just 0.95

-- instance MoveSpecies Blizzard where
  -- typeOf _ = Ice
  -- category _ = Special
  -- pps _ = 5
  -- basepower _ = Just 110
  -- accuracy _ = Just 0.7

-- instance MoveSpecies BodySlam where
  -- typeOf _ = Normal
  -- category _ = Physical
  -- pps _ = 15
  -- basepower _ = Just 85

-- instance MoveSpecies BrickBreak where
  -- typeOf _ = Fighting
  -- category _ = Physical
  -- pps _ = 15
  -- basepower _ = Just 75

-- instance MoveSpecies Crunch where
  -- typeOf _ = Dark
  -- category _ = Physical
  -- pps _ = 15
  -- basepower _ = Just 80

-- instance MoveSpecies DragonPulse where
  -- typeOf _ = Dragon
  -- category _ = Special
  -- pps _ = 10
  -- basepower _ = Just 85

-- instance MoveSpecies Earthquake where
  -- typeOf _ = Ground
  -- category _ = Physical
  -- pps _ = 10
  -- basepower _ = Just 100
  -- makesContact _ = False

-- instance MoveSpecies FakeOut where
  -- typeOf _ = Normal
  -- category _ = Physical
  -- pps _ = 10
  -- basepower _ = Just 40
  -- priority _ = 3

-- instance MoveSpecies FireBlast where
  -- typeOf _ = Fire
  -- category _ = Special
  -- pps _ = 5
  -- basepower _ = Just 110
  -- accuracy _ = Just 0.85

-- instance MoveSpecies FocusBlast where
  -- typeOf _ = Fighting
  -- category _ = Special
  -- pps _ = 5
  -- basepower _ = Just 120
  -- accuracy _ = Just 0.7

-- instance MoveSpecies HydroPump where
  -- typeOf _ = Water
  -- category _ = Special
  -- pps _ = 5
  -- basepower _ = Just 110
  -- accuracy _ = Just 0.8

-- instance MoveSpecies IceBeam where
  -- typeOf _ = Ice
  -- category _ = Special
  -- pps _ = 10
  -- basepower _ = Just 90

-- instance MoveSpecies IceShard where
  -- typeOf _ = Ice
  -- category _ = Physical
  -- pps _ = 30
  -- basepower _ = Just 40
  -- makesContact _ = False
  -- priority _ = 1

-- instance MoveSpecies IronTail where
  -- typeOf _ = Steel
  -- category _ = Physical
  -- pps _ = 15
  -- basepower _ = Just 100
  -- accuracy _ = Just 0.75

-- instance MoveSpecies LeafStorm where
  -- typeOf _ = Grass
  -- category _ = Special
  -- pps _ = 5
  -- basepower _ = Just 130
  -- accuracy _ = Just 0.9

-- instance MoveSpecies SeedBomb where
  -- typeOf _ = Grass
  -- category _ = Physical
  -- pps _ = 15
  -- basepower _ = Just 80
  -- makesContact _ = False

-- instance MoveSpecies SleepPowder where
  -- typeOf _ = Grass
  -- pps _ = 15
  -- accuracy _ = Just 0.75

-- instance MoveSpecies SludgeBomb where
  -- typeOf _ = Poison
  -- category _ = Special
  -- pps _ = 10
  -- basepower _ = Just 90

-- instance MoveSpecies Thunderbolt where
  -- typeOf _ = Electric
  -- category _ = Special
  -- pps _ = 15
  -- basepower _ = Just 90

-- instance MoveSpecies VoltTackle where
  -- typeOf _ = Electric
  -- category _ = Physical
  -- pps _ = 15
  -- basepower _ = Just 120

-- instance MoveSpecies WaterSpout where
  -- typeOf _ = Water
  -- category _ = Special
  -- pps _ = 5
  -- basepower _ = Just 150
