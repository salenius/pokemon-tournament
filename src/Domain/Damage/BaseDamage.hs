{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Domain.Damage.BaseDamage where

import Domain.Attribute.HeldItem
import Domain.Attribute.MoveExecution
import Domain.Attribute.HP
import Domain.Attribute.Counterparty
import Domain.Attribute.Damage
import Control.Lens

data BaseDamageMove battle =
  BaseDamageMove Int 
  | Acrobatics (AcrobaticsMove battle)
  | Avalanche (AvalancheMove battle)
  | GrassKnot (GrassKnotMove battle)
  | Payback (PaybackMove battle)
  | WaterSpout (WaterSpoutMove battle)

data AcrobaticsMove battle = AcrobaticsMove
  {
    _acrobaticsUserHeldItem :: battle -> Maybe HeldItem
  , _acrobaticsBasepower :: Int
  }

data AvalancheMove battle = AvalancheMove
  {
    _avalanchePrevMove :: battle -> Maybe PreviousStrike,
    _avalancheBasepower :: Int
  }

newtype GrassKnotMove battle = GrassKnotMove
  {
    _grassKnotPokemonWeight :: Counterparty -> battle -> Double
  }

data PaybackMove battle = PaybackMove
  {
    _paybackPrevMove :: battle -> Maybe PreviousStrike
  , _paybackBasepower :: Int
  }

data WaterSpoutMove battle = WaterSpoutMove
  {
    _waterSpoutPokemonHp :: Counterparty -> battle -> HP,
    _waterSpoutBasepower :: Int
  }

makeLenses ''AcrobaticsMove
makeLenses ''AvalancheMove
makeLenses ''GrassKnotMove
makeLenses ''PaybackMove
makeLenses ''WaterSpoutMove
makePrisms ''BaseDamageMove

reduce :: BaseDamageMove battle -> battle -> Damage
reduce (BaseDamageMove x) _ = Damage . fromIntegral $ x
reduce (Acrobatics a) b =
  Damage $
  fromIntegral $
  case view acrobaticsUserHeldItem a b of
    Nothing        -> 2 * view acrobaticsBasepower a 
    Just FlyingGem -> 2 * view acrobaticsBasepower a
    Just _         -> view acrobaticsBasepower a
reduce (Avalanche a) b =
  Damage $
  fromIntegral $
  case view avalanchePrevMove a b >>= preview (_CausesDamage . damageCaused) of
    Just (Damage 0) -> view avalancheBasepower a
    Just _          -> 2 * view avalancheBasepower a
    Nothing         -> view avalancheBasepower a
reduce (GrassKnot g) b =
  Damage $
  fromIntegral $
  case view grassKnotPokemonWeight g Target b of
    ((< 10) -> True) -> 20
    ((< 25) -> True) -> 40
    ((< 50) -> True) -> 60
    ((< 100) -> True) -> 80
    ((< 200) -> True) -> 100
    _ -> 120
reduce (Payback p) b =
  Damage $
  fromIntegral $
  case view paybackPrevMove p b of
    Nothing -> view paybackBasepower p
    Just _ -> 2 * view paybackBasepower p
reduce (WaterSpout w) b =
  Damage $
  (*) (fromIntegral $ view waterSpoutBasepower w) $
  hpPct $
  view waterSpoutPokemonHp w Target b
