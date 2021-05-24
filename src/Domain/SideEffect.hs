{-# LANGUAGE RankNTypes #-}


module Domain.SideEffect where

import Domain.Common hiding (poisoned,burned,paralyzed)
import Domain.Logic
import Attribute
import Attribute.HP
import Attribute.Damage
import Types.BuiltIn
import Stats.Base
import Prelude hiding (or,and)
import Control.Lens (view, review,prism',Prism')

class (CounterpartyGetter m,
       MoldBreakerGetter m,
       PokemonAttrGetter m,
       Monad m) => SideEffect m where
  pokemonTypeIs :: Player -> TypeOf -> m Bool
  targetType :: TypeOf -> m Bool
  targetType tp = getTarget >>= flip pokemonTypeIs tp
  userType :: TypeOf -> m Bool
  userType tp = getUser >>= flip pokemonTypeIs tp
  pokemonAbilityIs :: Player -> (Player -> m Bool) -> m Bool
  notIn :: (TypeOf -> m Bool) -> [TypeOf] -> m Bool
  pokemonIs :: Player -> Ailment -> m Bool
  targetIs :: Ailment -> m Bool
  targetIs ai = getTarget >>= flip pokemonIs ai
  userIs :: Ailment -> m Bool
  userIs ai = getUser >>= flip pokemonIs ai
  damageDealt :: m Damage
  leaveOneHp :: Player -> m Action
  is :: (m Bool -> m Bool) -> m Bool -> m Bool
  is = ($)
  isNot :: (m Bool -> m Bool) -> m Bool -> m Bool
  isNot cpf b = do
    x <- cpf b
    return $ x /= True
  (%) :: Double -> m Action -> m Action
  ---- Logic
  no :: ((Player -> m Bool) -> m Bool) -> (Player -> m Bool) -> m Bool
    --- HP
  hpCurrently :: Player -> m HP
  hpCurrentlyInPct :: Player -> m Double
  hpCurrentlyInPct plr = do
    x <- hpCurrently plr
    return $ hpPct x
  hpCurrentlyLessThanPct :: Double -> Player -> m Bool
  hpCurrentlyLessThanPct pct plr = do
    x <- hpCurrentlyInPct plr
    return $ pct > x
  hpCurrentlyEqualToPct :: Double -> Player -> m Bool
  hpCurrentlyEqualToPct pct plr = do
    x <- hpCurrentlyInPct plr
    return $ pct == x
  fullHp :: Player -> m Bool
  fullHp plr = hpCurrentlyEqualToPct 1.0 plr
  zeroHp :: Player -> m Bool
  zeroHp plr =  hpCurrentlyEqualToPct 0 plr
  --- Stat related
  loweredStats :: Player -> m [(ModifStat, Int)]
  --- Abilities
  comatose :: Player -> m Bool
  immunity :: Player -> m Bool
  synchronize :: Player -> m Bool
  corrosion :: Player -> m Bool
  safeguard :: Player -> m Bool
  intimidate :: Player -> m Bool
  cloudNine :: Player -> m Bool
  airLock :: Player -> m Bool
  clearBody :: Player -> m Bool
  hyperCutter :: Player -> m Bool
  whiteSmoke :: Player -> m Bool
  sandStream :: Player -> m Bool
  snowWarning, magmaArmor, vitalSpirit, insomnia :: Player -> m Bool
  drizzle, contrary :: Player -> m Bool
  draught, suctionCups :: Player -> m Bool
  bigPecks, moxie, rockHead :: Player -> m Bool
  keenEye, ownTempo, sturdy :: Player -> m Bool
  innerFocus, waterVeil, waterBubble :: Player -> m Bool
  steadfast, cuteCharm :: Player -> m Bool
  limber, static, poisonTouch, flameBody, poisonPoint, cutemCharm :: Player -> m Bool
  -- Items
  rockyHelmet, lightClay :: Player -> m Bool
  focusSash, sitrusBerry, whiteHerb :: Player -> m Bool
  
  -- Ops
  getDamage :: m Damage
  putAilment :: Counterparty -> Ailment -> m Action
  putConfusion :: Counterparty -> m Action
  setSandstorm :: m Action
  setHail :: m Action
  setRainy :: m Action
  setSunny :: m Action
  lowerStat :: Counterparty -> ModifStat -> Int -> m Action
  raiseStat :: Counterparty -> ModifStat -> Int -> m Action
  putFlinch :: Counterparty -> m Action
  addPctOfMaxHp :: Counterparty -> Double -> m Action
  dropItem :: Counterparty -> m Action
  putDamage :: Counterparty -> m Action
  putRecoil :: Double -> m Action
  drainDamage :: Double -> m Action
  dealtDamage :: m Bool
  dealtDamage = do
    Damage dam <- getDamage
    return $ dam > 0
  putSwitch :: Counterparty -> m Action
  putLeechSeed :: Counterparty -> m Action
  putLightScreen :: Counterparty -> Int -> m Action
  putReflect :: Counterparty -> Int -> m Action
  recoverStats :: Counterparty -> [(ModifStat, Int)] -> m Action
  
infixr 6 %


ailmented' :: Prism' BadAilment' a -> a -> Ailment
ailmented' f g = review (_BadAilment' .  f) g
poisoned = ailmented' _Poisoned Poisoned'
burned = ailmented' _Burned Burned'
paralyzed = ailmented' _Paralyzed Paralyzed'
healthy = review _Healthy Healthy'
frozen = ailmented' _Frozen Frozen'
sleep = ailmented' _Sleep Sleep'

poisonTarget :: SideEffect m => m Action
poisonTarget =
  ailmentedTarget
  (targetType `notIn` [Steel,Poison] `or` userHas corrosion)
  (targetHas `no` immunity) poisoned poisonUser

poisonUser :: SideEffect m => m Action
poisonUser = ailmentedUser (userType `notIn` [Steel,Poison]) (userHas `no` immunity) poisoned

paralyzeUser :: SideEffect m => m Action
paralyzeUser = do
  ailmentedUser (userType Electric) (userHas `no` limber) paralyzed

paralyzeTarget :: SideEffect m => m Action
paralyzeTarget =
  ailmentedTarget (targetType Electric) (targetHas `no` limber) paralyzed paralyzeUser

burnTarget :: SideEffect m => m Action
burnTarget =
  ailmentedTarget (targetType Fire)
  (targetHas `no` waterVeil `or` targetHas `no` waterBubble) 
  burned burnUser

burnUser :: SideEffect m => m Action
burnUser =
  ailmentedUser (userType Fire) (userHas `no` waterVeil `or` userHas `no` waterBubble) burned

freezeTarget :: SideEffect m => m Action
freezeTarget = do
  targetHas `no` comatose `or` targetHas `no` magmaArmor `or` userHas moldBreaker
    `and'` targetType `notIn` [Ice]
    `and'` targetIs healthy
    `and'` targetHas `no` safeguard
    --> putAilment Target frozen

freezeUser :: SideEffect m => m Action
freezeUser = do
  userHas `no` comatose `or` userHas `no` magmaArmor
    `and` userType `notIn` [Ice]
    `and` userIs healthy
    `and` userHas `no` safeguard
    --> putAilment User frozen

sleepTarget :: SideEffect m => m Action
sleepTarget = do
  targetHas `no` comatose `and` targetHas `no` vitalSpirit `and` targetHas `no` insomnia `or` userHas moldBreaker
    `and'` targetIs healthy
    `and'` targetHas `no` safeguard
    --> putAilment Target sleep

sleepUserByItself :: SideEffect m => m Action
sleepUserByItself = do
  userHas `no` comatose `and` userHas `no` vitalSpirit `and` userHas `no` insomnia
    `and` userHas `no` safeguard
    --> putAilment Target sleep

healUser :: SideEffect m => m Action
healUser = putAilment User healthy

healTarget :: SideEffect m => m Action
healTarget = putAilment Target healthy


switchUser :: SideEffect m => m Action
switchUser = do
  userHas `no` suctionCups `or` userHas zeroHp
    --> do
      putSwitch User
      afterSwitchingUser

switchTarget :: SideEffect m => m Action
switchTarget = do
  targetHas `no` suctionCups `or` userHas moldBreaker `or` userHas zeroHp
    --> do
      putSwitch Target
      afterSwitchingTarget


confuseTarget :: SideEffect m => m Action
confuseTarget = do
  userHas moldBreaker `or` targetHas `no` ownTempo
    --> putConfusion Target


-- This will be activated only after all effects have been played
afterAllEffects :: SideEffect m => m Action
afterAllEffects = do
  usr   <- getUser
  trgt  <- getTarget
  stats <- loweredStats usr
  stats' <- loweredStats trgt
  userHas whiteHerb
    --> do
     recoverStats User stats
     dropItem User
  targetHas whiteHerb
    --> do
      recoverStats Target stats
      dropItem Target

lowerUserStatByTarget :: SideEffect m => ModifStat -> Int -> m Action
lowerUserStatByTarget Attack' x = do
  userHas `no` hyperCutter `and`
    userHas `no` clearBody `and`
    userHas `no` whiteSmoke
    --> lowerStat User Attack' x
lowerUserStatByTarget Defence' x = do
  userHas `no` bigPecks `and`
    userHas `no` clearBody `and`
    userHas `no` whiteSmoke
    --> lowerStat User Defence' x
lowerUserStatByTarget Accuracy' x = do
   userHas `no` keenEye `and`
    userHas `no` clearBody `and`
    userHas `no` whiteSmoke
    --> lowerStat User Accuracy' x
lowerUserStatByTarget y x = do 
  userHas `no` clearBody `and`
    userHas `no` whiteSmoke
    --> lowerStat User y x

lowerUserStatByUser  :: SideEffect m => ModifStat -> Int -> m Action
lowerUserStatByUser mstat lvl = do
  userHas contrary
    --> raiseStat User mstat lvl
    `othercase` lowerStat User mstat lvl


increaseUserStat :: SideEffect m => ModifStat -> Int -> m Action
increaseUserStat mstat lvl = do
  userHas contrary
    --> lowerStat User mstat lvl
    `othercase` raiseStat User mstat lvl

increaseTargetStat  :: SideEffect m => ModifStat -> Int -> m Action
increaseTargetStat mstat lvl = do
  targetHas contrary `and` userHas `no` contrary
    --> lowerStat Target mstat lvl
    `othercase` raiseStat Target mstat lvl

lowerTargetStat :: SideEffect m => ModifStat -> Int -> m Action
lowerTargetStat Attack' x = do
  targetHas `no` hyperCutter `and` userHas `no` moldBreaker
    --> lowerTargetStat' Attack' x
lowerTargetStat Defence' x = do
  targetHas `no` bigPecks `and` userHas `no` moldBreaker
    --> lowerTargetStat' Defence' x
lowerTargetStat Accuracy' x = do
   targetHas `no` keenEye `and` userHas `no` moldBreaker
    --> lowerTargetStat' Accuracy' x
lowerTargetStat y x = lowerTargetStat' y x

thawUser :: SideEffect m => m Action
thawUser = do
  userIs frozen
    --> putAilment User healthy

thawTarget :: SideEffect m => m Action
thawTarget = do
  targetIs frozen
    --> putAilment Target healthy

flinchTarget :: SideEffect m => m Action
flinchTarget = do
  targetHas `no` innerFocus `or`
    userHas moldBreaker
    --> targetHas steadfast
    --> do
         putFlinch Target
         increaseTargetStat Speed' 1
    `othercase` putFlinch Target

contactEffect :: SideEffect m => m Action
contactEffect = do
  targetHas static
    --> 30 % paralyzeUser
  targetHas poisonPoint
    --> 30 % poisonUser
  targetHas flameBody
    --> 30 % burnUser
  targetHas rockyHelmet
    --> addPctOfMaxHp Target (-0.1667)
  userHas poisonTouch
    --> 30 % poisonTarget
  targetHas cuteCharm
    --> 30 % infatuateTarget

leechSeedTarget :: SideEffect m => m Action
leechSeedTarget = do
  targetType `notIn` [Grass]
    --> putLeechSeed Target

infatuateTarget :: SideEffect m => m Action
infatuateTarget = undefined

causeRecoil :: SideEffect m => Double -> m Action
causeRecoil pct = do
  userHas `no` rockHead
    --> dealDamageToUser (putRecoil pct)

--- This is used when calculating damage losses to the user
--- in total
dealDamageToUser :: SideEffect m => m Action -> m Action
dealDamageToUser damAction = do
  userHas `no` (hpCurrentlyLessThanPct 1.0)
    --> userHas sturdy `or` userHas focusSash
      --> dealDamageToUserWithFullHp damAction 
      `othercase` afterDamageForUser damAction
    `othercase` afterDamageForUser damAction

--- This is used when calculating damage losses to the targettype
--- in total
dealDamageToTarget :: SideEffect m => m Action
dealDamageToTarget = do
  targetHas `no` (hpCurrentlyLessThanPct 1.0)
    --> targetHas sturdy `or` targetHas focusSash
      --> dealDamageToTargetWithFullHp 
      `othercase` afterDamageForTarget
    `othercase` afterDamageForTarget


setLightScreenForUser :: SideEffect m => m Action
setLightScreenForUser = do
  userHas lightClay
    --> putLightScreen User 8
    `othercase`
    putLightScreen User 5

setReflectForUser :: SideEffect m => m Action
setReflectForUser = do
  userHas lightClay
    --> putReflect User 8
    `othercase`
    putReflect User 5


---- HP helpers

dealDamageToTargetWithFullHp :: SideEffect m => m Action
dealDamageToTargetWithFullHp = do
  targetHas ohkoHit `and` targetHas sturdy `and` userHas `no` moldBreaker
    --> targetDoes leaveOneHp
    `othercase` targetHas ohkoHit `and` targetHas focusSash
    --> do
      dropItem Target
      targetDoes leaveOneHp
  afterDamageForTarget

dealDamageToUserWithFullHp :: SideEffect m => m Action -> m Action
dealDamageToUserWithFullHp damAction = do
  userHas ohkoHit `and` userHas sturdy
    --> userDoes leaveOneHp
    `othercase` userHas ohkoHit `and` userHas focusSash
      --> do
        dropItem User
        userDoes leaveOneHp
  afterDamageForUser damAction

-- All damage loss logic here for target in case
-- Focus Sash and Sturdy have been gone through
afterDamageForTarget :: SideEffect m => m Action
afterDamageForTarget = do
  putDamage Target
  targetHas sitrusBerry `and` targetHas `no` zeroHp
    --> addPctOfMaxHp Target 0.25
  userHas moxie `and` targetHas zeroHp
    --> increaseUserStat Attack' 1
  targetHas zeroHp
    --> switchTarget

afterDamageForUser :: SideEffect m => m Action -> m Action
afterDamageForUser damAction = do
  damAction
  userHas sitrusBerry `and` userHas `no` zeroHp
    --> addPctOfMaxHp User 0.25
  userHas zeroHp
    --> switchUser


ohkoHit :: SideEffect m => Player -> m Bool
ohkoHit plr = do
  Damage dam <- damageDealt
  curHp <- fromIntegral . view currentHp <$> hpCurrently plr
  return $ dam >= curHp

----- Switch helpers

afterSwitchingUser :: SideEffect m => m Action
afterSwitchingUser =  afterSwitchingPlayer userHas targetHas lowerTargetStat

afterSwitchingTarget :: SideEffect m => m Action
afterSwitchingTarget = afterSwitchingPlayer targetHas userHas lowerUserStatByTarget

--------------------------

---- Helper function
ailmentedTarget :: SideEffect m => m Bool -> m Bool -> Ailment -> m Action -> m Action
ailmentedTarget noBlockingAbility noBlockingTypes ailm next = do
  x <- noBlockingTypes
   `and'` targetHas `no` comatose `or` noBlockingAbility `or` userHas moldBreaker
   `and'` targetIs healthy
   `and'` targetHas `no` safeguard
    --> putAilment Target ailm
  userHas synchronize `and` isApplied x
    --> next
  where
    isApplied x = (==) x <$> applied

ailmentedUser :: SideEffect m => m Bool -> m Bool -> Ailment -> m Action
ailmentedUser noBlockingAbility noBlockingTypes ailm = do
  noBlockingTypes `or` userHas corrosion
   `and'` targetHas `no` comatose `or` noBlockingAbility
   `and'` targetIs healthy
   `and'` targetHas `no` safeguard
    --> putAilment Target ailm


afterSwitchingPlayer
  :: SideEffect m =>
  ((Player -> m Bool) -> m Bool)
  -> ((Player -> m Bool) -> m Bool)
  -> (ModifStat -> Int -> m Action)
  -> m Action
afterSwitchingPlayer enteringPokemonHas existingPokemonHas lowerS  = do
  enteringPokemonHas intimidate `and'`
    existingPokemonHas `no` clearBody
    `or` existingPokemonHas `no` hyperCutter
    `or` existingPokemonHas `no` whiteSmoke
    --> lowerS Attack' 1
  enteringPokemonHas sandStream `and'`
    existingPokemonHas `no` cloudNine
    `or` existingPokemonHas `no` airLock
    --> setSandstorm
  enteringPokemonHas snowWarning `and'`
    existingPokemonHas `no` cloudNine
    `or` existingPokemonHas `no` airLock
    --> setHail
  enteringPokemonHas draught `and'`
    existingPokemonHas `no` cloudNine
    `or` existingPokemonHas `no` airLock
    --> setSunny
  enteringPokemonHas drizzle `and'`
    existingPokemonHas `no` cloudNine
    `or` existingPokemonHas `no` airLock
    --> setRainy

lowerTargetStat' :: SideEffect m => ModifStat -> Int -> m Action
lowerTargetStat' mstat lvl =
  targetHas `no` clearBody `and`
    targetHas `no` whiteSmoke `or'`
    userHas moldBreaker
    --> userHas `no` moldBreaker `and` targetHas contrary
    --> raiseStat Target mstat lvl
    `othercase` lowerStat Target mstat lvl

