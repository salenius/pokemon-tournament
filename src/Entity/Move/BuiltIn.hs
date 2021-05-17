{-# LANGUAGE RankNTypes #-}

module Entity.Move.BuiltIn where

import Entity.Move.Algebra

type BasicMove = forall mv. (MoveBasic mv) => mv ()
type EffectiveMove = forall mv. (EffectWhenHits mv) => mv ()

airSlash :: EffectiveMove
blizzard :: EffectiveMove
bodySlam :: EffectiveMove
bulletPunch :: BasicMove
closeCombat ::  EffectiveMove
crunch :: EffectiveMove
dragonPulse :: BasicMove
earthPower :: EffectiveMove
earthquake :: BasicMove
extremeSpeed :: BasicMove
fireBlast :: EffectiveMove
firePunch :: EffectiveMove
flamethrower :: EffectiveMove
focusBlast :: EffectiveMove
hydroPump :: BasicMove
iceBeam :: EffectiveMove
iceFang ::  EffectiveMove
icePunch :: EffectiveMove
iceShard ::  BasicMove
ironTail :: EffectiveMove
leafStorm :: EffectiveMove
machPunch :: EffectiveMove
psychic :: EffectiveMove
quickAttack :: BasicMove
seedBomb :: BasicMove
shadowBall :: EffectiveMove
thunderbolt :: EffectiveMove
thunderPunch :: EffectiveMove
voltTackle :: EffectiveMove
waterfall :: EffectiveMove
waterPulse :: EffectiveMove
xScissor :: BasicMove
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
