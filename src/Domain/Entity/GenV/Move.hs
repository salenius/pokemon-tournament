{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Domain.Entity.GenV.Move where

import Prelude hiding ((.), break)
import Domain.Entity.Common
import qualified Domain.Entity.Move as M
import Domain.Entity.Move (name,pp,basepower,accuracy,physical,special,status,makesContact,typeof,priority,alwaysHits)
import Domain.Entity.GenV.TypeOf as T
import Domain.Entity.GenV.Effect as Eff
import Domain.Entity.GenV.Damage as D
import Domain.Entity.GenV.Success as S
import Domain.Entity.GenV.Hit
import Data.Map (fromList)

type Move ch cn = M.Move TypeOf ch MoveSuccess MoveHit (DamageLogic cn) (Effect Battle) 

instance SideEffectAlgebra (Move ch cn) where
  -- causeAilment cp ai x = Branch (M.SideEffect (CounterpartySpecific cp (CauseAilment ai))) x
   causeAilment cp ai = wrapEffect (CounterpartySpecific cp (CauseAilment ai)) 
   causeEffectWithCount cp eff i = cpWrapEffect cp (CauseEffectWithCount i eff)
   causeFlinch cp = cpWrapEffect cp CauseFlinch
   causeRecoil d = wrapEffect (CauseRecoil d)
   raiseStat cp s i = cpWrapEffect cp (RaiseStat s i)
   dropStat cp s i = cpWrapEffect cp (DropStat s i)
   changeWeather w = wrapEffect (ChangeWeather w)
   leechSeedPokemon cp = cpWrapEffect cp LeechSeed
   protectPokemon cp = cpWrapEffect cp Protect
   causeConfusion cp i j = cpWrapEffect cp (Confuse i j)
   lightScreenUp cp i = cpWrapEffect cp (Screen (Put i) LightScreen)
   reflectUp cp i = cpWrapEffect cp (Screen (Put i) Reflect)
   breakLightScreen cp = cpWrapEffect cp (Screen Smash LightScreen)
   breakReflect cp = cpWrapEffect cp (Screen Smash Reflect)
   switchPokemon cp = cpWrapEffect cp SwitchPokemon
   thaw cp = cpWrapEffect cp ThawPokemon
   lockMove i j = wrapEffect (LockMove i j)
   recoverHP cp d = cpWrapEffect cp (RecoverHP d)
   drainDamage d = wrapEffect (DrainDamage d)
   equalizeHPs = wrapEffect EqualizeHPs
   callRandomMove am = wrapEffect (CallRandomMove am)

instance SideEffectCombine (Move ch cn) where
  withProbability d eff mv = f (eff mv)
    where
      f (Branch (M.SideEffect x) y) = Branch (M.SideEffect (WithProbability d x)) y

instance SuccessAlgebra (Move ch cn) where
  onlyUsedOnFirstTurn = wrapSuccess OnlyUsedOnFirstTurn
  failsIfUserFaster = wrapSuccess FailsIfUserFaster
  chargesFirstTurn c = wrapSuccess (ChargesFirstTurn c)
  failsIfTargetHasntAttacked = wrapSuccess FailsIfTargetHasntAttacked
  failsIfTargetAilmented a = wrapSuccess (FailsIfTargetAilmented a)
  failsIfUserAilmented a = wrapSuccess (FailsIfUserAilmented a)
  droppingSuccessRate r = wrapSuccess (DroppingSuccessRate r)
  failsIfTargetNotAttacking = wrapSuccess FailsIfTargetNotAttacking

instance DamageAlgebra (Move ch cn) where
  multiplyBasepowerIf cond d = wrapDamage (BasepowerCalc (MultiplyIfCondition d cond))
  determineBasepowerBy attr f = wrapDamage (BasepowerCalc (DetermineBy (Factor attr f)))
  increasedCriticalHit i = wrapDamage (CriticalHitCalc (IncreasedCriticalHit i))
  multiStrikeMove m = wrapDamageLogic (MultistrikeMove (fromList m) NormalDamage)
  ohko = wrapDamageLogic (ConstantDamageCalc OHKO)
  constantDamage i = wrapDamageLogic (ConstantDamageCalc (ConstantDamage i))
  multipleOfPreviousDamage d = wrapDamageLogic (ConstantDamageCalc (MultipleOfPreviousDamage d))
  hurtBySandstorm = wrapDamage (AffectedByWeather HurtBySandstorm)
  hurtByHail = wrapDamage (AffectedByWeather HurtByHail)
  hurtByRain = wrapDamage (AffectedByWeather HurtByRain)

instance HitAlgebra (Move ch cn) where
  bypassesAccuracyCheckInWeather w = Branch (M.Accuracy (M.DependentProbability (BypassesAccuracyCheckInWeather w)))

instance M.MoveTypeAlgebra (Move ch cn) TypeOf where
  typeof t = Branch (M.TypeOf t)

wrapDamage :: (Damage cn -> Damage cn) -> Move ch cn -> Move ch cn
wrapDamage d = wrapDamageLogic (DamageCalc (d NormalDamage))

wrapDamageLogic :: DamageLogic cn -> Move ch cn -> Move ch cn
wrapDamageLogic d = Branch (M.DamageDealing d)

-- wrapEffect :: SideEffectAlgebra m => m -> Move ch cn -> Move ch cn
wrapEffect y x = Branch (M.SideEffect y) x

(%) :: Double -> (Move ch cn -> Move ch cn) -> Move ch cn -> Move ch cn
(%) d = withProbability (d / 100)

infixr 9 %

cpWrapEffect cp x = wrapEffect (CounterpartySpecific cp x)

wrapSuccess x = Branch (M.MoveSucceeds x)

end :: Move ch cn
end = End

electric :: Move ch cn -> Move ch cn
electric = M.typeof Electric

steel :: Move ch cn -> Move ch cn
steel = typeof Steel

normal :: Move ch cn -> Move ch cn
normal = typeof Normal

fighting :: Move ch cn -> Move ch cn
fighting = typeof Fighting

flying :: Move ch cn -> Move ch cn
flying = typeof Flying

water :: Move ch cn -> Move ch cn
water = typeof Water

fire :: Move ch cn -> Move ch cn
fire = typeof Fire

grass :: Move ch cn -> Move ch cn
grass = typeof Grass

ground :: Move ch cn -> Move ch cn
ground = typeof Ground

rock :: Move ch cn -> Move ch cn
rock = typeof Rock

poisonType :: Move ch cn -> Move ch cn
poisonType = typeof T.Poison

bug :: Move ch cn -> Move ch cn
bug = typeof Bug

ice :: Move ch cn -> Move ch cn
ice = typeof Ice

psychicType :: Move ch cn -> Move ch cn
psychicType = typeof Psychic

dark :: Move ch cn -> Move ch cn
dark = typeof Dark

ghost :: Move ch cn -> Move ch cn
ghost = typeof Ghost

dragon :: Move ch cn -> Move ch cn
dragon = typeof Dragon

user = User
target = Target

paralyze c = causeAilment c Paralyze
burn c = causeAilment c Burn
poison c = causeAilment c Eff.Poison
freeze c i j = causeAilment c (Eff.Freeze i j) 
sleep c i j = causeAilment c (Eff.Sleep i j)

lower :: Counterparty -> Stat -> Int -> Move ch cn -> Move ch cn
lower cp s i = dropStat cp s i

raise :: Counterparty -> Stat -> Int -> Move ch cn -> Move ch cn
raise cp s i = raiseStat cp s i

flinch c = causeFlinch c

setUp :: Counterparty -> Screen -> Move ch cn -> Move ch cn
setUp cp s = case s of
  LightScreen -> lightScreenUp cp 5
  Reflect -> reflectUp cp 5

break :: Counterparty -> Screen -> Move ch cn -> Move ch cn
break cp s = case s of
  LightScreen -> breakLightScreen cp
  Reflect -> breakReflect cp

start :: WeatherType -> Move ch cn -> Move ch cn
start w = changeWeather (Eff.Weather w 5)

---

voltTackle :: Move ch cn
voltTackle =
  name "Volt Tackle" .
  electric .
  physical .
  pp 15 .
  basepower 120 .
  10 % paralyze target .
  makesContact .
  causeRecoil 0.333 $
  end

ironTail :: Move ch cn
ironTail =
  name "Iron Tail" .
  steel .
  physical .
  pp 15 .
  basepower 100 .
  accuracy 0.75 .
  makesContact .
  30 % lower target SDefence 1 $
  end

thunderbolt :: Move ch cn
thunderbolt =
  name "Thunderbolt" .
  electric .
  special .
  pp 15 .
  basepower 90 .
  makesContact .
  10 % paralyze target $
  end

iceFang :: Move ch cn
iceFang =
  name "Ice Fang" .
  ice .
  physical .
  pp 15 .
  basepower 65 .
  accuracy 0.95 .
  makesContact .
  10 % freeze target 2 5 .
  10 % flinch target $
  end

flamethrower :: Move ch cn
flamethrower =
  name "Flamethrower" .
  fire .
  special .
  pp 15 .
  basepower 90 .
  10 % burn target $
  end

earthquake :: Move ch cn
earthquake = 
  name "Earthquake" .
  ground .
  physical .
  pp 10 .
  basepower 100 $
  end

iceBeam :: Move ch cn
iceBeam = 
  name "Ice Beam" .
  ice .
  special .
  pp 10 .
  basepower 90 .
  10 % freeze target 2 5 $
  end

xScissor :: Move ch cn
xScissor = 
  name "X-Scissor" .
  bug .
  physical .
  pp 15 .
  makesContact .
  basepower 80 $
  end

shadowBall  :: Move ch cn
shadowBall =
  name "Shadow Ball" .
  ghost .
  special .
  pp 15 .
  basepower 80 .
  20 % lower target SDefence 1 $
  end

dragonPulse  :: Move ch cn
dragonPulse = 
  name "Dragon Pulse" .
  dragon .
  special .
  pp 10 .
  basepower 85 $
  end

psychic  :: Move ch cn
psychic = 
  name "Psychic" .
  psychicType .
  special .
  pp 10 .
  basepower 90 .
  10 % lower target SDefence 1 $
  end

closeCombat  :: Move ch cn
closeCombat = 
  name "Close Combat" .
  fighting .
  physical .
  pp 5 .
  basepower 120 .
  makesContact .
  lower user Defence 1 .
  lower user SDefence 1 $
  end

extremeSpeed  :: Move ch cn
extremeSpeed = 
  name "ExtremeSpeed" .
  normal .
  physical .
  pp 5 .
  basepower 80 .
  makesContact .
  priority 2 $
  end

quickAttack  :: Move ch cn
quickAttack = 
  name "Quick Attack" .
  normal .
  physical .
  pp 30 .
  basepower 40 .
  makesContact .
  priority 1 $
  end

machPunch  :: Move ch cn
machPunch = 
  name "Mach Punch" .
  fighting .
  physical .
  pp 30 .
  basepower 40 .
  makesContact .
  priority 1 $
  end

iceShard  :: Move ch cn
iceShard = 
  name "Ice Shard" .
  ice .
  physical .
  pp 30 .
  basepower 40 .
  makesContact .
  priority 1 $
  end

icePunch  :: Move ch cn
icePunch = 
  name "Ice Punch" .
  ice .
  physical .
  pp 15 .
  basepower 75 .
  makesContact .
  10 % freeze target 2 5 $
  end

firePunch  :: Move ch cn
firePunch = 
  name "Fire Punch" .
  fire .
  physical .
  pp 15 .
  basepower 75 .
  makesContact .
  10 % burn target $
  end

thunderPunch  :: Move ch cn
thunderPunch = 
  name "Thunder Punch" .
  electric .
  physical .
  pp 15 .
  basepower 75 .
  makesContact .
  10 % paralyze target $
  end

focusBlast  :: Move ch cn
focusBlast = 
  name "Focus Blast" .
  fighting .
  special .
  pp 5 .
  basepower 120 .
  accuracy 0.7 .
  10 % lower target SDefence 1 $
  end

blizzard :: Move ch cn
blizzard =
  name "Blizzard" .
  ice .
  special .
  pp 5 .
  basepower 110 .
  accuracy 0.7 .
  10 % freeze target 2 5 .
  bypassesAccuracyCheckInHail $
  end

fireBlast  :: Move ch cn
fireBlast =
  name "Fire Blast" .
  fire .
  special .
  pp 5 .
  basepower 110 .
  accuracy 0.85 .
  10 % burn target $
  end

waterfall  :: Move ch cn
waterfall =
  name "Waterfall" .
  water .
  physical .
  pp 15 .
  basepower 80 .
  20 % flinch target $
  end

waterPulse  :: Move ch cn
waterPulse =
  name "Water Pulse" .
  water .
  special .
  pp 20 .
  basepower 60 .
  20 % causeConfusion target 2 5 $
  end

airSlash  :: Move ch cn
airSlash =
  name "Air Slash" .
  flying .
  special .
  pp 15 .
  accuracy 0.95 .
  basepower 75 .
  makesContact .
  30 % flinch target $
  end

seedBomb  :: Move ch cn
seedBomb =
  name "Seed Bomb" .
  grass .
  physical .
  pp 15 .
  basepower 80 $
  end

earthPower  :: Move ch cn
earthPower =
  name "Earth Power" .
  ground .
  special .
  pp 10 .
  basepower 90 .
  10 % lower target SDefence 1 $
  end

leafStorm  :: Move ch cn
leafStorm =
  name "Leaf Storm" .
  grass .
  special .
  pp 5 .
  basepower 130 .
  accuracy 0.9 .
  lower user SAttack 2 $
  end

zenHeadbutt  :: Move ch cn
zenHeadbutt =
  name "Zen Headbutt" .
  psychicType .
  physical .
  pp 15 .
  basepower 80 .
  accuracy 0.9 .
  20 % flinch target $
  end

bulletPunch  :: Move ch cn
bulletPunch =
  name "Bullet Punch" .
  steel .
  physical .
  pp 30 .
  basepower 40 .
  makesContact .
  priority 1 $
  end

crunch  :: Move ch cn
crunch =
  name "Crunch" .
  dark .
  physical .
  pp 15 .
  basepower 80 .
  makesContact .
  20 % lower target Defence 1 $
  end

bodySlam  :: Move ch cn
bodySlam =
  name "Body Slam" .
  normal .
  physical .
  pp 15 .
  basepower 85 .
  makesContact .
  30 % paralyze target $
  end

hydroPump  :: Move ch cn
hydroPump =
  name "Hydro Pump" .
  water .
  special .
  pp 5 .
  basepower 110 .
  accuracy 0.8 $
  end

sludgeBomb  :: Move ch cn
sludgeBomb =
  name "Sludge Bomb" .
  poisonType .
  special .
  pp 10 .
  basepower 90 .
  30 % poison target $
  end

woodHammer  :: Move ch cn
woodHammer =
  name "Wood Hammer" .
  grass .
  physical .
  pp 15 .
  makesContact .
  basepower 120 .
  causeRecoil 0.333 $
  end

superpower  :: Move ch cn
superpower =
  name "Superpower" .
  fighting .
  physical .
  pp 5 .
  basepower 120 .
  makesContact .
  lower user Attack 1 .
  lower user Defence 1 $
  end
 
uTurn :: Move ch cn 
uTurn = 
  name "U-Turn" .
  physical .
  pp 20 .
  basepower 70 .
  makesContact .
  switchPokemon user $
  end

dracoMeteor  :: Move ch cn
dracoMeteor =
  name "Draco Meteor" .
  dragon .
  special .
  pp 5 .
  basepower 130 .
  accuracy 0.9 .
  lower user SAttack 2 $
  end

dragonClaw  :: Move ch cn
dragonClaw =
  name "Dragon Claw" .
  dragon .
  physical .
  pp 15 .
  basepower 80 $
  end

flashCannon  :: Move ch cn
flashCannon =
  name "Flash Cannon" .
  ice .
  special .
  pp 10 .
  basepower 80 .
  10 % lower target SDefence 1 $
  end

rockSlide  :: Move ch cn
rockSlide =
  name "Rock Slide" .
  rock .
  physical .
  pp 10 .
  basepower 75 .
  accuracy 0.9 .
  30 % flinch target $
  end

headSmash  :: Move ch cn
headSmash =
  name "Head Smash" .
  rock .
  physical .
  pp 5 .
  basepower 150 .
  accuracy 0.8 .
  makesContact .
  causeRecoil 0.5 $
  end

hammerArm  :: Move ch cn
hammerArm =
  name "Hammer Arm" .
  fighting .
  physical .
  pp 10 .
  basepower 100 .
  accuracy 0.9 .
  makesContact .
  lower user Speed 1 $
  end

icyWind  :: Move ch cn
icyWind =
  name "Icy Wind" .
  ice .
  special .
  pp 15 .
  basepower 55 .
  accuracy 0.95 .
  lower target Speed 1 $
  end

aquaJet  :: Move ch cn
aquaJet =
  name "Aqua Jet" .
  water .
  physical .
  pp 30 .
  basepower 40 .
  makesContact .
  priority 1 $
  end

signalBeam  :: Move ch cn
signalBeam =
  name "Signal Beam" .
  bug .
  special .
  pp 15 .
  basepower 75 .
  10 % causeConfusion target 2 5 $
  end

muddyWater  :: Move ch cn
muddyWater =
  name "Muddy Water" .
  water .
  special .
  pp 10 .
  basepower 90 .
  accuracy 0.85 .
  30 % lower target Accuracy 1 $
  end

darkPulse  :: Move ch cn
darkPulse =
  name "Dark Pulse" .
  dark .
  special .
  pp 15 .
  basepower 80 .
  20 % flinch target $
  end

heatWave  :: Move ch cn
heatWave =
  name "Heat Wave" .
  fire .
  special .
  pp 10 .
  basepower 95 .
  accuracy 0.9 .
  10 % burn target $
  end

energyBall  :: Move ch cn
energyBall =
  name "Energy Ball" .
  grass .
  special .
  pp 10 .
  basepower 90 .
  10 % lower target SDefence 1 $
  end

braveBird  :: Move ch cn
braveBird =
  name "Brave Bird" .
  flying .
  physical .
  pp 15 .
  basepower 120 .
  makesContact .
  causeRecoil 0.333 $
  end

bugBuzz  :: Move ch cn
bugBuzz =
  name "Bug Buzz" .
  bug .
  special .
  pp 10 .
  basepower 90 .
  10 % lower target SDefence 1 $
  end

auraSphere  :: Move ch cn
auraSphere =
  name "Aura Sphere" .
  fighting .
  special .
  pp 20 .
  basepower 80 .
  alwaysHits $
  end

sleepPowder  :: Move ch cn
sleepPowder =
  name "Sleep Powder" .
  grass .
  status .
  pp 15 .
  accuracy 0.75 .
  sleep target 2 5 $
  end

rainDance  :: Move ch cn
rainDance =
  name "Rain Dance" .
  water .
  status .
  pp 5 .
  alwaysHits .
  start Eff.Rainy $
  end

sandstorm  :: Move ch cn
sandstorm =
  name "Sandstorm" .
  rock .
  status .
  pp 5 .
  alwaysHits .
  start Eff.Sandstorm $
  end

sunnyDay  :: Move ch cn
sunnyDay =
  name "Sunny Day" .
  fire .
  status .
  pp 5 .
  alwaysHits .
  start Eff.Sunny $
  end

swordsDance  :: Move ch cn
swordsDance =
  name "Swords Dance" .
  normal .
  status .
  pp 20 .
  alwaysHits .
  raise user Attack 2 $
  end

toxic :: Move ch cn
toxic =
  name "Toxic" .
  poisonType .
  status .
  pp 10 .
  accuracy 0.9 .
  poison target $
  end

stoneEdge  :: Move ch cn
stoneEdge =
  name "Stone Edge" .
  rock .
  physical .
  pp 5 .
  basepower 100 .
  accuracy 0.8 .
  increasedCriticalHit 1 $
  end

quiverDance  :: Move ch cn
quiverDance =
  name "Quiver Dance" .
  bug .
  status .
  pp 20 .
  alwaysHits .
  raise user SAttack 1 .
  raise user SDefence 1 .
  raise user Speed 1 $
  end

brickBreak  :: Move ch cn
brickBreak =
  name "Brick Break" .
  fighting .
  physical .
  pp 15 .
  basepower 75 .
  makesContact .
  break target LightScreen .
  break target Reflect $
  end

lightScreen  :: Move ch cn
lightScreen =
  name "Light Screen" .
  psychicType .
  status .
  pp 30 .
  alwaysHits .
  setUp user LightScreen $
  end

reflect  :: Move ch cn
reflect =
  name "Reflect" .
  psychicType .
  status .
  pp 30 .
  alwaysHits .
  setUp user Reflect $
  end

fakeOut :: Move ch cn
fakeOut =
  name "Fake Out" .
  normal .
  physical .
  pp 10 .
  basepower 40 .
  priority 3 .
  makesContact .
  onlyUsedOnFirstTurn .
  flinch target $
  end

rockBlast  :: Move ch cn
rockBlast =
  name "Rock Blast" .
  rock .
  physical .
  pp 10 .
  basepower 25 .
  accuracy 0.9 .
  multiStrikeMove [(2, 0.375), (3, 0.375), (4, 0.125), (5, 0.125)] $
  end

leechSeed  :: Move ch cn
leechSeed =
  name "Leech Seed" .
  grass .
  status .
  pp 10 .
  accuracy 0.9 .
  leechSeedPokemon target $
  end

acrobatics  :: Move ch cn
acrobatics =
  name "Acrobatics" .
  flying .
  physical .
  makesContact .
  pp 15 .
  basepower 55 .
  multiplyBasepowerIf UserHasNoItem 2 $
  end

grassKnot  :: Move ch cn
grassKnot =
  name "grassKnot" .
  grass .
  special .
  pp 20 .
  makesContact .
  determineBasepowerBy (TargetHas Weight) f $
  end
  where
    f w 
      | w < 10 = 20
      | w < 25 = 40
      | w < 50 = 60
      | w < 100 = 80
      | w < 200 = 100
      | otherwise = 120

avalanche  :: Move ch cn
avalanche =
  name "Avalanche" .
  ice .
  physical .
  pp 10 .
  basepower 60 .
  multiplyBasepowerIf UserReceivedDamage 2 .
  makesContact .
  priority (-4) $
  end

waterSpout  :: Move ch cn
waterSpout =
  name "Water Spout" .
  water .
  special .
  pp 5 .
  determineBasepowerBy (UserHas HPPercent) (floor . (*) 150) $
  end

solarBeam  :: Move ch cn
solarBeam =
  name "Solar Beam" .
  grass .
  special .
  pp 10 .
  basepower 120 .
  hurtBySandstorm .
  hurtByRain .
  hurtByHail .
  chargesFirstTurn (AttacksIfWeather S.Sunny) $
  end

metalBurst  :: Move ch cn
metalBurst =
  name "Metal Burst" .
  steel .
  physical .
  pp 10 .
  multipleOfPreviousDamage 2 .
  failsIfTargetHasntAttacked $
  end

scald :: Move ch cn
scald =
  name "Scald" .
  water .
  special .
  pp 15 .
  basepower 80 .
  30 % burn target .
  thaw user $
  end

rest :: Move ch cn
rest =
  name "Rest" .
  psychicType .
  status .
  pp 10 .
  alwaysHits .
  sleep user 2 5 .
  recoverHP user 1 $
  end

surf :: Move ch cn
surf =
  name "Surf" .
  water .
  special .
  pp 15 .
  basepower 90 $
  end

outrage  :: Move ch cn
outrage =
  name "Outrage" .
  dragon .
  physical .
  pp 10 .
  basepower 120 .
  makesContact .
  lockMove 2 3 $
  end

flareBlitz  :: Move ch cn
flareBlitz =
  name "Flare Blitz" .
  fire .
  physical .
  pp 10 .
  basepower 120 .
  10 % burn target .
  causeRecoil 0.333 .
  makesContact .
  thaw user $
  end

wildCharge  :: Move ch cn
wildCharge =
  name "Wild Charge" .
  electric .
  physical .
  pp 15 .
  basepower 90 .
  makesContact .
  causeRecoil 0.25 $
  end

protect  :: Move ch cn
protect =
  name "Protect" .
  normal .
  status .
  pp 10 .
  alwaysHits .
  priority 4 .
  droppingSuccessRate 2 .
  protectPokemon user $
  end

yawn :: Move ch cn
yawn =
  name "Yawn" .
  normal .
  status .
  pp 10 .
  alwaysHits .
  failsIfTargetAilmented S.Burned .
  failsIfTargetAilmented S.Paralyzed .
  failsIfTargetAilmented S.Poisoned .
  failsIfTargetAilmented S.Frozen .
  failsIfTargetAilmented S.Sleep .
  causeAilmentWithCount target (Eff.Sleep 2 5) 1 $
  end

willOWisp  :: Move ch cn
willOWisp =
  name "Will-O-Wisp" .
  fire .
  status .
  pp 15 .
  accuracy 0.85 .
  burn target $
  end

gigaDrain  :: Move ch cn
gigaDrain =
  name "Giga Drain" .
  grass .
  special .
  pp 10 .
  basepower 75 .
  drainDamage 0.5 $
  end

sheerCold  :: Move ch cn
sheerCold =
  name "Sheer Cold" .
  ice .
  special .
  pp 5 .
  accuracy 0.30 .
  ohko $
  end

payback  :: Move ch cn
payback =
  name "Payback" .
  dark .
  physical .
  pp 10 .
  basepower 50 .
  makesContact .
  multiplyBasepowerIf (Or UserMovedLast TargetUsedItem) 2 $
  end

painSplit :: Move ch cn
painSplit =
  name "Pain Split" .
  normal .
  status .
  pp 20 .
  alwaysHits .
  equalizeHPs $
  end

sleepTalk :: Move ch cn
sleepTalk =
  name "Sleep Talk" .
  normal .
  status .
  pp 10 .
  alwaysHits .
  failsIfUserAilmented S.Healthy .
  failsIfUserAilmented S.Burned .
  failsIfUserAilmented S.Paralyzed .
  failsIfUserAilmented S.Poisoned .
  failsIfUserAilmented S.Frozen .
  callRandomMove OwnMoves $
  end

suckerPunch :: Move ch cn
suckerPunch =
  name "Sucker Punch" .
  dark .
  physical .
  basepower 80 .
  pp 5 .
  priority 1 .
  makesContact .
  failsIfTargetNotAttacking $
  end
