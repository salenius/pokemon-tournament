{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Entity.Move.BuiltIn where

import Data.Maybe
import Attribute.MoveExecution
import Attribute.Damage
import Attribute.HP
import Attribute.Weather
import Types.BuiltIn
import Entity.Move.Algebra
import Prelude hiding (break)
import Domain.Common

type BasicMove = forall mv. (MoveBasic mv) => mv ()
type EffectiveMove = forall mv. (EffectWhenHits mv) => mv ()

acrobatics :: (BattleStrikeGetter m, MoveBasic m, BasepowerModification m) => m ()
airSlash :: EffectiveMove
aquaJet :: BasicMove
auraSphere :: BasicMove
avalanche :: (BasepowerModification m) => m ()
blizzard :: EffectiveMove
bodySlam :: EffectiveMove
braveBird :: EffectiveMove
brickBreak :: EffectiveMove
bugBuzz :: EffectiveMove
bulletPunch :: BasicMove
closeCombat ::  EffectiveMove
crunch :: EffectiveMove
darkPulse :: EffectiveMove
dracoMeteor :: EffectiveMove
dragonClaw :: BasicMove
dragonPulse :: BasicMove
earthPower :: EffectiveMove
earthquake :: BasicMove
energyBall :: EffectiveMove
extremeSpeed :: BasicMove
fakeOut :: (ModifyDamage m, EffectWhenHits m) => m ()
fireBlast :: EffectiveMove
firePunch :: EffectiveMove
flamethrower :: EffectiveMove
flareBlitz :: (ModifyDamage m, EffectWhenHits m) => m ()
flashCannon :: EffectiveMove
focusBlast :: EffectiveMove
gigaDrain :: EffectiveMove
grassKnot :: (BasepowerModification m) => m ()
hammerArm :: EffectiveMove
headSmash :: EffectiveMove
heatWave :: EffectiveMove
hydroPump :: BasicMove
iceBeam :: EffectiveMove
iceFang ::  EffectiveMove
icePunch :: EffectiveMove
iceShard ::  BasicMove
icyWind :: EffectiveMove
ironTail :: EffectiveMove
leafStorm :: EffectiveMove
leechSeed :: EffectiveMove
lightScreen :: EffectiveMove
machPunch :: EffectiveMove
metalBurst :: (BasepowerModification m, ModifyDamage m) => m ()
muddyWater :: EffectiveMove
outrage :: (ModifyDamage m, EffectWhenHits m) => m ()
payback :: (BasepowerModification m, ModifyDamage m) => m ()
protect :: EffectiveMove
psychic :: EffectiveMove
quickAttack :: BasicMove
quiverDance :: EffectiveMove
rainDance :: EffectiveMove
reflect :: EffectiveMove
rest :: EffectiveMove
rockBlast :: (ModifyDamage m) => m ()
rockSlide :: EffectiveMove
sandstorm :: EffectiveMove
scald :: (ModifyDamage m, EffectWhenHits m) => m ()
seedBomb :: BasicMove
shadowBall :: EffectiveMove
sheerCold :: (ModifyDamage m, BasepowerModification m) => m ()
signalBeam :: EffectiveMove
sleepPowder :: EffectiveMove
sludgeBomb :: EffectiveMove
solarBeam :: (ModifyDamage m, BasepowerModification m) => m ()
stoneEdge :: (ModifyDamage m) => m ()
sunnyDay :: EffectiveMove
superpower :: EffectiveMove
surf :: BasicMove
swordsDance :: EffectiveMove
thunderbolt :: EffectiveMove
thunderPunch :: EffectiveMove
toxic :: EffectiveMove
uTurn :: EffectiveMove
voltTackle :: EffectiveMove
waterfall :: EffectiveMove
waterPulse :: EffectiveMove
waterSpout :: (BasepowerModification m) => m ()
wildCharge :: EffectiveMove
willOWisp :: EffectiveMove
woodHammer :: EffectiveMove
xScissor :: BasicMove
yawn :: EffectiveMove
zenHeadbutt :: EffectiveMove
  
voltTackle = do
  electric
  physical
  pp 15
  basepower 120
  10 % paralyze target
  recoil 0.333

ironTail = do
  steel
  physical
  pp 15
  basepower 100
  accuracy 0.75
  30 % lower target sdefence 1

thunderbolt = do
  electric
  special
  pp 15
  basepower 90
  10 % paralyze target

iceFang = do
  ice
  physical
  pp 15
  basepower 65
  accuracy 0.95
  10 % freeze target
  10 % flinch target

flamethrower = do
  fire
  special
  pp 15
  basepower 90
  10 % burn target

earthquake = do
  ground
  physical
  pp 10
  basepower 100
  makes `no` contact

iceBeam = do
  ice
  special
  pp 10
  basepower 90
  10 % freeze target

xScissor = do
  bug
  physical
  pp 15
  basepower 80

shadowBall = do
  ghost
  special
  pp 15
  basepower 80
  20 % lower target sdefence 1

dragonPulse = do
  dragon
  special
  pp 10
  basepower 85

psychic = do
  psychicType
  special
  pp 10
  basepower 90
  10 % lower target sdefence 1

closeCombat = do
  fighting
  physical
  pp 5
  basepower 120
  lower user defence 1
  lower user sdefence 1

extremeSpeed = do
  normal
  physical
  pp 5
  basepower 80
  priority 2

quickAttack = do
  normal
  physical
  pp 30
  basepower 40
  priority 1

machPunch = do
  fighting
  physical
  pp 30
  basepower 40
  priority 1

iceShard = do
  ice
  physical
  pp 30
  basepower 40
  priority 1

icePunch = do
  ice
  physical
  pp 15
  basepower 75
  10 % freeze target

firePunch = do
  fire
  physical
  pp 15
  basepower 75
  10 % burn target

thunderPunch = do
  electric
  physical
  pp 15
  basepower 75
  10 % paralyze target

focusBlast = do
  fighting
  special
  pp 5
  basepower 120
  accuracy 0.7
  10 % lower target sdefence 1

blizzard = do
  ice
  special
  pp 5
  basepower 110
  accuracy 0.7
  10 % freeze target

fireBlast = do
  fire
  special
  pp 5
  basepower 110
  accuracy 0.85
  10 % burn target

waterfall = do
  water
  physical
  pp 15
  basepower 80
  20 % flinch target

waterPulse = do
  water
  special
  pp 20
  basepower 60
  20 % confuse target

airSlash = do
  flying
  special
  pp 15
  accuracy 0.95
  basepower 75
  30 % flinch target

seedBomb = do
  grass
  physical
  pp 15
  basepower 80
  makes `no` contact

earthPower = do
  ground
  special
  pp 10
  basepower 90
  10 % lower target sdefence 1

leafStorm = do
  grass
  special
  pp 5
  basepower 130
  accuracy 0.9
  lower user sattack 2

zenHeadbutt = do
  psychicType
  physical
  pp 15
  basepower 80
  accuracy 0.9
  20 % flinch target

bulletPunch = do
  steel
  physical
  pp 30
  basepower 40
  priority 1

crunch = do
  dark
  physical
  pp 15
  basepower 80
  20 % lower target defence 1

bodySlam = do
  normal
  physical
  pp 15
  basepower 85
  30 % paralyze target

hydroPump = do
  water
  special
  pp 5
  basepower 110
  accuracy 0.8

sludgeBomb = do
  poisonType
  special
  pp 10
  basepower 90
  30 % poison target

woodHammer = do
  grass
  physical
  pp 15
  basepower 120
  recoil 0.333

superpower = do
  fighting
  physical
  pp 5
  basepower 120
  lower user attack 1
  lower user defence 1

uTurn = do
  bug
  physical
  pp 20
  basepower 70
  switch user

dracoMeteor = do
  dragon
  special
  pp 5
  basepower 130
  accuracy 0.9
  lower user sattack 2

dragonClaw = do
  dragon
  physical
  pp 15
  basepower 80
  
flashCannon = do
  ice
  special
  pp 10
  basepower 80
  10 % lower target sdefence 1

rockSlide = do
  rock
  physical
  pp 10
  basepower 75
  accuracy 0.9
  makes `no` contact
  30 % flinch target

headSmash = do
  rock
  physical
  pp 5
  basepower 150
  accuracy 0.8
  recoil 0.5

hammerArm = do
  fighting
  physical
  pp 10
  basepower 100
  accuracy 0.9
  lower user speed 1

icyWind = do
  ice
  special
  pp 15
  basepower 55
  accuracy 0.95
  lower target speed 1

aquaJet = do
  water
  physical
  pp 30
  basepower 40
  priority 1

signalBeam = do
  bug
  special
  pp 15
  basepower 75
  10 % confuse target

muddyWater = do
  water
  special
  pp 10
  basepower 90
  accuracy 0.85
  30 % lower target pokemonAccuracy 1

darkPulse = do
  dark
  special
  pp 15
  basepower 80
  20 % flinch target

heatWave = do
  fire
  special
  pp 10
  basepower 95
  accuracy 0.9
  10 % burn target

energyBall = do
  grass
  special
  pp 10
  basepower 90
  10 % lower target sdefence 1

braveBird = do
  flying
  physical
  pp 15
  basepower 120
  recoil 0.333

bugBuzz = do
  bug
  special
  pp 10
  basepower 90
  10 % lower target sdefence 1

auraSphere = do
  fighting
  special
  pp 20
  basepower 80
  alwaysHits

sleepPowder = do
  grass
  status
  pp 15
  accuracy 0.75
  sleep target

rainDance = do
  water
  status
  pp 5
  alwaysHits
  start rain

sandstorm = do
  rock
  status
  pp 5
  alwaysHits
  start sandStorm

sunnyDay = do
  fire
  status
  pp 5
  alwaysHits
  start sunlight

swordsDance = do
  normal
  status
  pp 20
  alwaysHits
  raise user attack 2

toxic = do
  poisonType
  status
  pp 10
  accuracy 0.9
  poison target

stoneEdge = do
  rock
  physical
  pp 5
  basepower 100
  accuracy 0.8
  makes `no` contact
  increasedCrit 1

quiverDance = do
  bug
  status
  pp 20
  alwaysHits
  raise user sattack 1
  raise user sdefence 1
  raise user speed 1

brickBreak = do
  fighting
  physical
  pp 15
  basepower 75
  break target lightScreen'
  break target reflect'

lightScreen = do
  psychicType
  status
  pp 30
  alwaysHits
  setUp user lightScreen'

reflect = do
  psychicType
  status
  pp 30
  alwaysHits
  setUp user reflect'

fakeOut = do
  normal
  physical
  pp 10
  basepower 40
  makes `no` contact
  priority 3
  turnCountOnBattle ((==) 0)
  flinch target

rockBlast = do
  rock
  physical
  pp 10
  basepower 25
  accuracy 0.9
  makes `no` contact
  multiHit [(2, 0.375), (3, 0.375), (4, 0.125), (5, 0.125)]

leechSeed = do
  grass
  status
  pp 10
  accuracy 0.9
  leechSeeded target

acrobatics = do
  flying
  physical
  pp 15
  basepower 55 `doubleIf` userHasNoItem
  where
    userHasNoItem = do
      x <- getUser
      y <- getHeldItem x
      return $ case y of
        Nothing -> True
        _       -> False

grassKnot = do
  grass
  special
  pp 20
  makes contact
  basepower 20 `adjustBy` targetWeight
  where
    targetWeight = do
      x <- getTarget
      y <- getWeight x
      return $ y
    adjustBy = modifyBy f
    f w _
      | w < 10 = 20
      | w < 25 = 40
      | w < 50 = 60
      | w < 100 = 80
      | w < 200 = 100
      | otherwise = 120

avalanche = do
  ice
  physical
  pp 10
  basepower 60 `doubleIf` userTookDamageFirst
  priority (-4)
  where
    userTookDamageFirst = do
      ps <- getPrevStrike
      return $ case _damageDealt <$> ps of
        Just _ -> True
        _      -> False

waterSpout = do
  water
  special
  pp 5
  basepower 150 `adjustBy` user'sCurrentHp
  where
    user'sCurrentHp = do
      x <- getUser
      y <- getHp x
      return $ hpPct y
    adjustBy = modifyBy f
    f hpPct' base = base * hpPct'

solarBeam = do
  grass
  special
  pp 10
  basepower 120 `adjustBy` weather
  chargeMove 1 `exceptIf` it'sSunny
  where
    weather = do
      x <- getWeather
      return $ x
    it'sSunny = do
      x <- getWeather
      let s = x == Sunny
      return s
    adjustBy = modifyBy f
    f wthr base = case wthr of
      Rainy -> 0.5 * base
      Hail -> 0.5 * base
      Sandstorm -> 0.5 * base
      _    -> base

metalBurst = do
  steel
  physical
  pp 10
  makes `no` contact
  pd <- previousDamage
  constantDamage pd
  where
    previousDamage = do
      x <- getPrevStrike
      let y = case x of
            Just m -> fromMaybe 0 $ do
              t <- x
              d <- _damageDealt t
              return $ damageToDouble $ d
            Nothing -> 0
      return y

scald = do
  water
  special
  pp 15
  basepower 80
  30 % burn target
  thaw user

rest = do
  psychicType
  status
  pp 10
  alwaysHits
  sleep user
  putHp user maxOutHp

surf = do
  water
  special
  pp 15
  basepower 90
  
outrage = do
  dragon
  physical
  pp 10
  basepower 120
  lockMove

flareBlitz = do
  fire
  physical
  pp 10
  basepower 120
  10 % burn target
  recoil 0.333
  thaw user

wildCharge = do
  electric
  physical
  pp 15
  basepower 90
  recoil 0.25

protect = do
  normal
  status
  pp 10
  alwaysHits
  priority 4
  protect' user

yawn = do
  normal
  status
  pp 10
  alwaysHits
  yawn' target
  
willOWisp = do
  fire
  status
  pp 15
  accuracy 0.85
  burn target

gigaDrain = do
  grass
  special
  pp 10
  basepower 75
  drainDamage ((*) 0.5)

sheerCold = do
  ice
  special
  pp 5
  accuracy 0.30
  usr <- getUser
  trgt <- getTarget
  lvl <- getLevel usr
  lvl' <- getLevel trgt
  ohko (basedOnLevelDiff (fromIntegral lvl) (fromIntegral lvl'))
  where
    basedOnLevelDiff lvl lvl' ac =
      ac + lvl / 100 - lvl' / 100

payback = do
  dark
  physical
  pp 10
  basepower 50 `doubleIf` targetAttacksFirst
  where
    targetAttacksFirst = do
      mv <- getPrevStrike
      return $ case mv of
        Just _ -> True
        _      -> False
