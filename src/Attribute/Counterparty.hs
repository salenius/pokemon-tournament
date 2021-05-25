{-# LANGUAGE DeriveFunctor #-}


module Attribute.Counterparty where


data Player = Player1 | Player2 deriving (Eq,Show,Read,Ord,Enum)

newtype UserHas a = UserHas a deriving (Eq,Show,Read,Ord,Functor)

newtype TargetHas a = TargetHas a deriving (Eq,Show,Read,Ord,Functor)

type CounterpartyHas a = Either (UserHas a) (TargetHas a)

data Counterparty =
  User
  | Target
  deriving (Eq,Show,Read,Ord)

class AsCounterparty cp where
  asCounterparty :: cp -> Counterparty

instance AsCounterparty (UserHas a) where
  asCounterparty _ = User

instance AsCounterparty (TargetHas a) where
  asCounterparty _ = Target


fromCounterparty :: Counterparty -> CounterpartyHas ()
fromCounterparty User = Left $ UserHas ()
fromCounterparty Target = Right $ TargetHas ()

opposite :: Counterparty -> Counterparty
opposite User = Target
opposite Target = User

isUser :: Counterparty -> Maybe Counterparty
isUser User = Just User
isUser _    = Nothing

isTarget :: Counterparty -> Maybe Counterparty
isTarget Target = Just Target
isTarget _    = Nothing

