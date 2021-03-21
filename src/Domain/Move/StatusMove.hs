module Domain.Move.StatusMove (
  StatusMoveBuiltIn(..),
  _StatusMoveBuiltIn
                              ) where

import Domain.Move.BuiltIn
import Control.Lens
  
data StatusMoveBuiltIn =
  AcidArmor'
  | LeechSeed'
  | LightScreen'
  | PainSplit'
  | QuiverDance'
  | Protect'
  | RainDance'
  | Reflect'
  | Rest'
  | Sandstorm'
  | SleepPowder'
  | SleepTalk'
  | SwordsDance'
  | Toxic'
  | WillOWisp'
  | Yawn'
  deriving (Eq,Show,Read,Ord,Enum)

_StatusMoveBuiltIn :: Prism' BuiltInMove StatusMoveBuiltIn
_StatusMoveBuiltIn = prism fromStatusMove (\mv -> case toStatusMove mv of
                                              Nothing -> Left mv
                                              Just m -> Right m)

toStatusMove :: BuiltInMove -> Maybe StatusMoveBuiltIn
toStatusMove AcidArmor = Just AcidArmor'
toStatusMove LeechSeed = Just LeechSeed'
toStatusMove LightScreen = Just LightScreen'
toStatusMove PainSplit = Just PainSplit'
toStatusMove QuiverDance = Just QuiverDance'
toStatusMove Protect = Just Protect'
toStatusMove RainDance = Just RainDance'
toStatusMove Reflect = Just Reflect'
toStatusMove Rest = Just Rest'
toStatusMove Sandstorm = Just Sandstorm'
toStatusMove SleepPowder = Just SleepPowder'
toStatusMove SleepTalk = Just SleepTalk'
toStatusMove SwordsDance = Just SwordsDance'
toStatusMove Toxic = Just Toxic'
toStatusMove WillOWisp = Just WillOWisp'
toStatusMove Yawn = Just Yawn'
toStatusMove _ = Nothing

fromStatusMove :: StatusMoveBuiltIn -> BuiltInMove
fromStatusMove AcidArmor' = AcidArmor
fromStatusMove LeechSeed' = LeechSeed
fromStatusMove LightScreen' = LightScreen
fromStatusMove PainSplit' = PainSplit
fromStatusMove QuiverDance' = QuiverDance
fromStatusMove Protect' = Protect
fromStatusMove RainDance' = RainDance
fromStatusMove Reflect' = Reflect
fromStatusMove Rest' = Rest
fromStatusMove Sandstorm' = Sandstorm
fromStatusMove SleepPowder' = SleepPowder
fromStatusMove SleepTalk' = SleepTalk
fromStatusMove SwordsDance' = SwordsDance
fromStatusMove Toxic' = Toxic
fromStatusMove WillOWisp' = WillOWisp
fromStatusMove Yawn' = Yawn
