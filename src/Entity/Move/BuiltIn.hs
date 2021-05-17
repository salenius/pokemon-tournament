module Entity.Move.BuiltIn where

import Entity.Move.Algebra

closeCombat ::  (EffectWhenHits mv) => mv ()
dragonPulse :: (MoveBasic mv) => mv ()
earthquake :: (MoveBasic mv) => mv ()
extremeSpeed :: (MoveBasic mv) => mv ()
flamethrower :: (EffectWhenHits mv) => mv ()
iceBeam :: (EffectWhenHits mv) => mv ()
iceFang ::  (EffectWhenHits mv) => mv ()
ironTail :: (EffectWhenHits mv) => mv ()
psychic :: (EffectWhenHits mv) => mv ()
shadowBall :: (EffectWhenHits mv) => mv ()
thunderbolt :: (EffectWhenHits mv) => mv ()
voltTackle :: (EffectWhenHits mv) => mv ()
xScissor :: (MoveBasic mv) => mv ()
  
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
