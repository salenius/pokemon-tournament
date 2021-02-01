module Domain.Data.Move where

import Domain.Attribute.TypeOf
import Domain.Attribute.Counterparty
import Domain.Attribute.Category
import Domain.Attribute.Choice
import Domain.Attribute.ModifStat
import Domain.Attribute.MoveExecution
import Domain.Attribute.Ailment
import qualified Domain.Attribute.Damage as D
import Domain.Attribute.Ability
import Domain.Attribute.HeldItem
import qualified Domain.Attribute.Weather as W
import Domain.Match.SideEffect
import Domain.Match.BattleEffect
import Domain.Match.Validation
import Domain.Match.Damage
import Domain.Match.MoveSucceeds
import qualified Domain.Match.Accuracy as Ac
import Domain.Entity.Move
import qualified Domain.Attribute.Weather as W
import Control.Lens (set,over,view)

acrobatics :: Move
acrobatics =
  assignName "Acrobatics" $
  modifyBaseDamage 55 cond (*2) $
  physicalMove 55 Flying mempty
  where
    cond = ConditionHolds $
      (User `heldItemIs` ((==) Nothing)) `Or`
      (User `heldItemIs` ((==) (Just FlyingGem)))

airSlash :: Move
airSlash =
  assignName "Air Slash" $
  assignAccuracy 0.95 $
  specialMove 75 Flying $ withProbability 0.3 $ causeFlinch

aquaJet :: Move
aquaJet =
  assignName "Aqua Jet" $
  assignPP 20 $
  assignPriority 1 $
  physicalMove 40 Water mempty

auraSphere :: Move
auraSphere =
  assignName "Aura Sphere" $
  assignPP 20 $
  alwaysHits $
  modifyOtherFactors (ImmunityIf bulletproof) $
  specialMove 80 Fighting mempty

avalanche :: Move
avalanche =
  assignName "Avalanche" $
  assignPP 10 $
  assignPriority (-4) $
  set (_StrikeMove . damage . _StandardDamage . baseDamage) m $
  physicalMove 60 Ice mempty
  where
    m = ModifiedBaseDamage $ ModifiedBaseDamage' 60 (*2) $ ConditionHolds v
    v = moveExecutionIs (\x -> damageCaused x /= D.Damage 0)

blizzard :: Move
blizzard =
  assignName "Blizzard" $
  assignPP 5 $
  over (necessities . hitProbability) (Ac.alwaysHitsInWeather W.Hail) $
  assignAccuracy 0.7 $
  specialMove 110 Ice $ withProbability 0.1 $ causeAilment Target Frozen

bodySlam :: Move
bodySlam =
  assignName "Body Slam" $
  modifyOtherFactors MinimizeAmplifies $
  physicalMove 85 Normal $ withProbability 0.3 $ causeAilment Target Paralyzed

braveBird :: Move
braveBird =
  assignName "Brave Bird" $
  physicalMove 120 Flying $ causeRecoil 0.33

bugBuzz :: Move
bugBuzz =
  assignName "Bug Buzz" $
  assignPP 10 $
  modifyOtherFactors (ImmunityIf soundproof) $
  specialMove 90 Bug $ withProbability 0.1 $ dropStat Target SDefence' 1

bulletPunch :: Move
bulletPunch =
  assignName "Bullet Punch" $
  assignPP 30 $
  assignPriority 1 $
  physicalMove 40 Steel mempty

brickBreak =
  assignName "Brick Break" $
  physicalMove 75 Fighting $ removeLightScreen Target <> removeReflect Target

closeCombat =
  assignName "Close Combat" $
  assignPP 5 $
  physicalMove 120 Fighting $
  dropStat User Defence' 1 <> dropStat User SDefence' 1

crunch = 
  assignName "Crunch" $
  physicalMove 80 Dark $ withProbability 0.2 $ dropStat Target Defence' 1 

darkPulse =
  assignName "Dark Pulse" $
  specialMove 80 Dark $ withProbability 0.2 causeFlinch

dracoMeteor =
  assignName "Draco Meteor" $
  assignPP 5 $
  assignAccuracy 0.9 $
  specialMove 130 Dragon $ dropStat User SDefence' 2 

dragonClaw =
  assignName "Dragon Claw" $
  physicalMove 80 Dragon mempty

dragonPulse =
  assignName "Dragon Pulse" $
  specialMove 85 Dragon mempty

earthPower =
  assignName "Earth Power" $
  assignPP 10 $
  specialMove 90 Ground $ withProbability 0.1 $ dropStat Target SDefence' 1

earthquake :: Move
earthquake =
  assignName "Earthquake" $
  modifyOtherFactors DigAmplifies $
  assignPP 10 . noContact $ physicalMove 100 Ground $ mempty

energyBall :: Move
energyBall =
  assignName "Energy Ball" $
  modifyOtherFactors (ImmunityIf bulletproof) $
  assignPP 10 $
  specialMove 90 Grass $ withProbability 0.1 $ dropStat Target SDefence' 1

extremeSpeed :: Move
extremeSpeed =
  assignName "Extreme Speed" $
  assignPP 5 $
  assignPriority 2 $
  physicalMove 80 Normal mempty

fakeOut :: Move
fakeOut =
  assignName "Fake Out" $
  assignChoiceProbability (ChoiceRule UseOnce) $
  assignPP 10 $
  assignPriority 3 $
  physicalMove 40 Normal causeFlinch

fireBlast :: Move
fireBlast =
  assignName "Fire Blast" $
  assignPP 5 $
  assignAccuracy 0.85 $
  specialMove 110 Fire $ withProbability 0.1 $ causeAilment Target Burned

firePunch :: Move
firePunch =
  assignName "Fir Ppunch" $
  physicalMove 75 Fire $ withProbability 0.1 $ causeAilment Target Burned

flamethrower :: Move
flamethrower =
  assignName "Flamethrower" $
  specialMove 90 Fire $ withProbability 0.1 $ causeAilment Target Burned

flareBlitz :: Move
flareBlitz =
  assignName "Flare Blitz" $
  physicalMove 120 Fire $
  thawOut User <>
  causeRecoil 0.33 <>
  (withProbability 0.1 $ causeAilment Target Burned)

flashCannon :: Move
flashCannon =
  assignName "Flash Cannon" $
  specialMove 80 Steel $ withProbability 0.1 $ dropStat Target SDefence' 1

focusBlast :: Move
focusBlast =
  assignName "Focus Blast" $
  assignPP 5 $
  modifyOtherFactors (ImmunityIf bulletproof) $
  assignAccuracy 0.7 $
  specialMove 120 Fighting $ withProbability 0.1 $ dropStat Target SDefence' 1

gigaDrain :: Move
gigaDrain =
  assignName "Giga Drain" $
  assignPP 10 $
  specialMove 75 Grass $ drainDamage 0.5

grassKnot :: Move
grassKnot =
  assignName "Grass Knot" $
  modifyBaseDamage 120 TargetWeight f $
  specialMove 20 Grass mempty
  where
    f x
      | x < 10 = 20
      | x < 25 = 40
      | x < 50 = 60
      | x < 100 = 80
      | x < 200 = 100
      | otherwise = 120

hammerArm :: Move
hammerArm =
  assignName "Hammer Arm" $
  assignPP 10 $
  assignAccuracy 0.9 $
  physicalMove 100 Fighting $ dropStat User Speed' 1

headSmash =
  assignName "Head Smash" $
  assignPP 5 $
  assignAccuracy 0.8 $
  physicalMove 150 Rock $ causeRecoil 0.5

heatWave =
  assignName "Heat Wave" $
  assignPP 10 $
  assignAccuracy 0.9 $
  specialMove 95 Fire  $ withProbability 0.1 $ causeAilment Target Burned

hydroPump =
  assignName "Hydro Pump" $
  assignPP 5 $
  assignAccuracy 0.8 $
  specialMove 110 Water mempty

iceBeam =
  assignName "Ice Beam" $
  assignPP 10 $
  specialMove 90 Ice $
  withProbability 0.1 $ causeAilment Target Frozen

iceFang =
  assignName "Ice Fang" $
  assignAccuracy 0.95 $
  specialMove 65 Ice $
  (withProbability 0.1 $ causeAilment Target Frozen) <>
  withProbability 0.1 causeFlinch

icePunch =
  assignName "Ice Punch" $
  physicalMove 75 Ice $ withProbability 0.1 $ causeAilment Target Frozen

iceShard =
  assignName "Ice Shard" $
  assignPP 30 $
  assignPriority 1 $
  noContact $
  physicalMove 40 Ice mempty

icyWind =
  assignName "Icy Wind" $
  assignAccuracy 0.95 $
  specialMove 55 Ice $ dropStat Target Speed' 1

ironTail =
  assignName "Iron Tail" $
  physicalMove 100 Steel $ withProbability 0.3 $ dropStat Target Defence' 1

leafStorm =
  assignName "Leaf Storm" $
  assignPP 5 $
  assignAccuracy 0.9 $
  specialMove 130 Grass $ dropStat User SDefence' 2 

leechSeed :: Move
leechSeed =
  assignName "Leech Seed" $
  assignPP 10 $
  assignAccuracy 0.9 $
  makeStatusMove Grass causeLeechSeeded

lightScreen :: Move
lightScreen =
  assignName "Light Screen" $
  assignPP 30 $
  alwaysHits $
  makeStatusMove Psychic $ forRounds 5 $ putLightScreen User

machPunch :: Move
machPunch =
  assignName "Mach Punch" $
  assignPP 30 $
  assignPriority 1 $
  physicalMove 40 Fighting mempty

metalBurst :: Move
metalBurst =
  assignName "Metal Burst" $
  assignPP 10 $
  noContact $
  set (_StrikeMove . damage) (FromPrevDamage $ FromPrevDamage' f (TypeAdvantage Steel)) $
  physicalMove 100 Steel mempty
  where
    f ex = damageCaused ex <> D.Damage 1.5

muddyWater :: Move
muddyWater =
  assignName "Muddy Water" $
  assignPP 10 $
  assignAccuracy 0.85 $
  specialMove 90 Water $ withProbability 0.3 $ dropStat Target Accuracy' 1

outrage :: Move
outrage =
  assignName "Outrage" $
  assignPP 10 $
  assignChoiceProbability (ChoiceRule LockableMove) $
  physicalMove 120 Dragon mempty

painSplit :: Move
painSplit =
  assignName "Pain Split" $
  assignPP 20 $
  alwaysHits $
  makeStatusMove Normal equalizeHPs

payback :: Move
payback =
  assignName "Payback" $
  assignPP 10 $
  modifyBaseDamage 50 (ConditionHolds v) (*2) $
  physicalMove 50 Dark mempty
  where
    v = userStrikesSecond `Or` targetUsedItem

psychic :: Move
psychic =
  assignName "Psychic" $
  assignPP 10 $
  specialMove 90 Psychic $
  withProbability 0.1 $ dropStat Target SDefence' 1

quickAttack =
  assignName "Quick Attack" $
  assignPP 30 $
  assignPriority 1 $
  physicalMove 40 Normal mempty

quiverDance =
  assignName "Quiver Dance" $
  assignPP 20 $
  alwaysHits $
  makeStatusMove Bug $
  increaseStat User SAttack' 1 <>
  increaseStat User SDefence' 1 <>
  increaseStat User Speed' 1

protect :: Move
protect =
  assignName "Protect" $
  assignPP 10 $
  alwaysHits $
  assignPriority 4 $
  over (necessities . executionSuccess) DecliningSuccess $
  makeStatusMove Normal protectUser

rainDance :: Move
rainDance =
  assignName "Rain Dance" $
  makeStatusMove Water $ forRounds 5 $ changeWeather W.Rainy

reflect :: Move
reflect =
  assignName "Reflect" $
  assignPP 20 $
  alwaysHits $
  makeStatusMove Psychic $ forRounds 5 $ putReflect User

rest :: Move
rest =
  assignName "Rest" $
  alwaysHits $
  makeStatusMove Normal $
  (causeAilment User Healthy `Chain` causeAilment User Sleep) <>
  addPercentOfHP User 1.00

rockBlast :: Move
rockBlast =
  assignName "RockBlast" $
  assignPP 10 $
  assignAccuracy 0.9 $
  set (_StrikeMove . damage) (defaultMultiStrike 25 Physical) $
  physicalMove 25 Rock mempty

rockSlide =
  assignName "Rock Slide" $
  assignPP 10 $
  assignAccuracy 0.9 $
  noContact $
  physicalMove 75 Rock $ withProbability 0.3 causeFlinch

sandstorm =
  assignName "Sandstorm" $
  assignPP 10 $
  makeStatusMove Rock $ forRounds 5 $ changeWeather W.Sandstorm

scald =
  assignName "Scald" $
  specialMove 80 Water $
  thawOut Target <>
  thawOut User <>
  (withProbability 0.3 $ causeAilment Target Burned)

seedBomb :: Move
seedBomb =
  assignName "Seed Bomb" $
  noContact $
  modifyOtherFactors (ImmunityIf bulletproof) $
  physicalMove 80 Grass mempty

shadowBall :: Move
shadowBall =
  assignName "Shadow Ball" $
  modifyOtherFactors (ImmunityIf bulletproof) $
  specialMove 80 Ghost $ withProbability 0.2 $ dropStat Target SDefence' 1

sheerCold :: Move
sheerCold =
  assignName "Sheer Cold" $
  over (necessities . hitProbability) (Ac.ChangesByFactor Ac.LevelGap) $
  set (_StrikeMove . damage) (OHKO . OHKO' $ TypeAdvantage Ice) $
  specialMove 200 Ice mempty

signalBeam :: Move
signalBeam =
  assignName "Signal Beam" $
  specialMove 75 Bug $ withProbability 0.1 $ causeConfusion Target

sleepPowder :: Move
sleepPowder =
  assignName "Sleep Powder" $
  assignAccuracy 0.75 $
  makeStatusMove Grass $ causeAilment Target Sleep

sleepTalk :: Move -> Move -> Move -> Move
sleepTalk m1 m2 m3 =
  assignName "Sleep Talk" $
  assignChoiceProbability (ChoiceRule $ OnlyPossibilityIf v) $
  alwaysHits $
  makeCallbackMove Normal [m1,m2,m3]
  where
    v = User `ailmentIs` ((==) Sleep)

sludgeBomb :: Move
sludgeBomb =
  assignName "Sludge Bomb" $
  assignPP 10 $
  modifyOtherFactors (ImmunityIf bulletproof) $
  specialMove 90 Poison $ withProbability 0.3 $ causeAilment Target Poisoned

solarBeam :: Move
solarBeam =
  assignName "Solar Beam" $
  assignPP 10 $
  set (necessities . executionSuccess) (NormalExecutionIf v ChargingMove) $
  specialMove 120 Grass mempty
  where
    v = weatherIs (\w -> w == W.Sunny)

stoneEdge :: Move
stoneEdge =
  assignName "Stone Edge" $
  assignPP 5 $
  assignAccuracy 0.8 $
  noContact $
  modifyCriticalHit f $
  physicalMove 120 Rock mempty
  where
    f (CriticalHitCalc x) = CriticalHitCalc $ x + 1
    f (ModifyCriticalHitCalc v f' c) = ModifyCriticalHitCalc v f' $ f c

suckerPunch :: Move
suckerPunch =
  assignName "Sucker Punch" $
  assignPriority 1 $
  assignPP 5 $
  set (necessities . executionSuccess) OpponentChoseStrikeMove $
  specialMove 70 Dark mempty

superpower :: Move
superpower =
  assignName "Superpower" $
  assignPP 5 $
  physicalMove 120 Fighting $
  dropStat User Defence' 1 <> dropStat User Attack' 1

surf :: Move
surf =
  assignName "Surf" $
  modifyOtherFactors DiveAmplifies $
  specialMove 90 Water mempty

swordsDance :: Move
swordsDance =
  assignName "Swords Dance" $
  assignPP 20 $
  alwaysHits $
  makeStatusMove Normal $ increaseStat User Attack' 2

thunderbolt =
  assignName "Thunderbolt" $
  specialMove 90 Electric $ withProbability 0.1 $ causeAilment Target Paralyzed

toxic :: Move
toxic =
  assignName "Toxic" $
  assignAccuracy 0.9 $
  assignPP 10 $
  makeStatusMove Poison $ causeAilment Target Poisoned

uTurn =
  assignName "U-Turn" $
  assignPP 20 $
  physicalMove 70 Bug $ switchPokemon User

voltTackle =
  assignName "Volt Tackle" $
  assignPP 10 $
  physicalMove 120 Electric $
  withProbability 0.1 (causeAilment Target Paralyzed) <>
          (causeRecoil 0.33) 

waterfall =
  assignName "Waterfall" $
  physicalMove 80 Water $ withProbability 0.2 causeFlinch

waterPulse =
  assignName "Water Pulse" $
  assignPP 20 $
  specialMove 60 Water $ withProbability 0.2 $ causeConfusion Target

waterSpout =
  assignName "Water Spout" $
  assignPP 5 $
  set (_StrikeMove . damage . _StandardDamage . baseDamage) m $
  specialMove 150 Water mempty
  where
    m = ModifiedBaseDamage $ ModifiedBaseDamage' 150 f UserHP
    f = \hpRatio -> 150 * hpRatio

wildCharge =
  assignName "Wild Charge" $
  physicalMove 90 Electric $ causeRecoil 0.25

willOWisp :: Move
willOWisp =
  assignName "Will-O-Wisp" $
  assignAccuracy 0.85 $
  makeStatusMove Fire $ causeAilment Target Burned

woodHammer =
  assignName "Wood Hammer" $
  physicalMove 120 Grass $ causeRecoil 0.33


xScissor =
  assignName "X-Scissor" $
  physicalMove 80 Bug mempty

yawn :: Move
yawn =
  assignName "Yawn" $
  assignPP 10 $
  alwaysHits $
  makeStatusMove Normal yawnTarget

zenHeadbutt =
  assignName "Zen Headbutt" $
  physicalMove 80 Psychic $ withProbability 0.2 causeFlinch

