{-# LANGUAGE TemplateHaskell, ViewPatterns #-}

module Domain.Damage.BaseDamage where

import Domain.Attribute.HeldItem
import Domain.Attribute.MoveExecution
import Domain.Attribute.HP
import Domain.Attribute.Counterparty
import Domain.Attribute.Damage
import Control.Lens
import Data.Functor.Contravariant

data BaseDamageMove battle =
  BaseDamageMove Int 
  | Acrobatics (AcrobaticsMove battle)
  | Avalanche (AvalancheMove battle)
  | GrassKnot (GrassKnotMove battle)
  | Payback (PaybackMove battle)
  | WaterSpout (WaterSpoutMove battle)

data AcrobaticsMove battle = AcrobaticsMove
  {
    _acrobaticsPokemonHeldItem :: Counterparty -> battle -> Maybe HeldItem
  , _acrobaticsBasepower :: Int
  }

instance Contravariant AcrobaticsMove where
  contramap f a =
    a {_acrobaticsPokemonHeldItem = \cp -> _acrobaticsPokemonHeldItem a cp . f}

data AvalancheMove battle = AvalancheMove
  {
    _avalanchePrevMove :: battle -> Maybe PreviousStrike,
    _avalancheBasepower :: Int
  }

instance Contravariant AvalancheMove where
  contramap f p = p {_avalanchePrevMove = _avalanchePrevMove p . f}

newtype GrassKnotMove battle = GrassKnotMove
  {
    _grassKnotPokemonWeight :: Counterparty -> battle -> Double
  }

instance Contravariant GrassKnotMove where
  contramap f a =
    a {_grassKnotPokemonWeight = \cp -> _grassKnotPokemonWeight a cp . f}

data PaybackMove battle = PaybackMove
  {
    _paybackPrevMove :: battle -> Maybe PreviousStrike
  , _paybackBasepower :: Int
  }

instance Contravariant PaybackMove where
  contramap f p = p {_paybackPrevMove = _paybackPrevMove p . f} 

data WaterSpoutMove battle = WaterSpoutMove
  {
    _waterSpoutPokemonHp :: Counterparty -> battle -> HP,
    _waterSpoutBasepower :: Int
  }

instance Contravariant WaterSpoutMove where
  contramap f w = w {_waterSpoutPokemonHp = \cp -> _waterSpoutPokemonHp w cp . f}

makeLenses ''AcrobaticsMove
makeLenses ''AvalancheMove
makeLenses ''GrassKnotMove
makeLenses ''PaybackMove
makeLenses ''WaterSpoutMove
makePrisms ''BaseDamageMove

instance Contravariant BaseDamageMove where
  contramap f (BaseDamageMove x) = BaseDamageMove x
  contramap f (Acrobatics a) = Acrobatics $ contramap f a
  contramap f (Avalanche a) = Avalanche $ contramap f a
  contramap f (GrassKnot a) = GrassKnot $ contramap f a
  contramap f (Payback a) = Payback $ contramap f a
  contramap f (WaterSpout a) = WaterSpout $ contramap f a

reduce :: BaseDamageMove battle -> battle -> Damage
reduce (BaseDamageMove x) _ = Damage . fromIntegral $ x
reduce (Acrobatics a) b =
  Damage $
  fromIntegral $
  case view acrobaticsPokemonHeldItem a User b of
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
