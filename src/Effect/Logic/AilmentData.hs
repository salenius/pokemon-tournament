module Effect.Logic.AilmentData where

import Attribute.Weather
import Attribute.Ability
import Attribute.Counterparty
import Types.Pokemon
import Control.Monad.Reader


data AilmentData = AilmentData
  {
    ability :: Counterparty -> PokemonAbility
  , typeOf :: Counterparty -> PokemonType
  , safeguardOn :: Counterparty -> Maybe Safeguard
  , weather :: Weather
  , existingAilment :: Counterparty -> Either () SomeAilment
  , ailment :: SomeAilment
  , affected :: Counterparty
  }

type Env a = ReaderT AilmentData Maybe a


type Safeguard = ()

data SomeAilment
  = Poison' | Burn' | Paralysis' | Freeze' | Sleep' deriving (Eq,Show,Read,Enum,Ord)
