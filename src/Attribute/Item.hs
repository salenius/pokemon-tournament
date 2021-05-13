{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}


module Attribute.Item where

import Attribute.Env
import GHC.Generics

newtype HeldItem = HeldItem { heldItemToString :: String } deriving (Eq,Show,Read,Ord,Generic)

newtype BattleItem = BattleItem { battleItemToString :: String } deriving (Eq,Show,Read,Ord,Generic)

newtype HealingItem = HealingItem { healingItemToString :: String } deriving (Eq,Show,Read,Ord,Generic)

type HeldItem' i = Prism' HeldItem i

data AirBalloon = AirBalloon deriving (Eq,Show,Read,Ord,Generic)
_AirBalloon :: HeldItem' AirBalloon
_AirBalloon = heldItem

data ChartiBerry = ChartiBerry deriving (Eq,Show,Read,Ord,Generic)
_ChartiBerry :: HeldItem' ChartiBerry
_ChartiBerry = heldItem

data ChoiceBand = ChoiceBand deriving (Eq,Show,Read,Ord,Generic)
_ChoiceBand :: HeldItem' ChoiceBand
_ChoiceBand = heldItem

data ChoiceScarf = ChoiceScarf deriving (Eq,Show,Read,Ord,Generic)
_ChoiceScarf :: HeldItem' ChoiceScarf
_ChoiceScarf = heldItem

data ExpertBelt = ExpertBelt deriving (Eq,Show,Read,Ord,Generic)
_ExpertBelt :: HeldItem' ExpertBelt
_ExpertBelt = heldItem

data FlyingGem = FlyingGem deriving (Eq,Show,Read,Ord,Generic)
_FlyingGem :: HeldItem' FlyingGem
_FlyingGem = heldItem

data FocusSash = FocusSash deriving (Eq,Show,Read,Ord,Generic)
_FocusSash :: HeldItem' FocusSash
_FocusSash = heldItem

data KingsRock = KingsRock deriving (Eq,Show,Read,Ord,Generic)
_KingsRock :: HeldItem' KingsRock
_KingsRock = heldItem

data Leftovers = Leftovers deriving (Eq,Show,Read,Ord,Generic)
_Leftovers :: HeldItem' Leftovers
_Leftovers = heldItem

data LifeOrb = LifeOrb deriving (Eq,Show,Read,Ord,Generic)
_LifeOrb :: HeldItem' LifeOrb
_LifeOrb = heldItem

data LightBall = LightBall deriving (Eq,Show,Read,Ord,Generic)
_LightBall :: HeldItem' LightBall
_LightBall = heldItem

data OccaBerry = OccaBerry deriving (Eq,Show,Read,Ord,Generic)
_OccaBerry :: HeldItem' OccaBerry
_OccaBerry = heldItem

data PowerHerb = PowerHerb deriving (Eq,Show,Read,Ord,Generic)
_PowerHerb :: HeldItem' PowerHerb
_PowerHerb = heldItem

data QuickClaw = QuickClaw deriving (Eq,Show,Read,Ord,Generic)
_QuickClaw :: HeldItem' QuickClaw
_QuickClaw = heldItem

data RazorClaw = RazorClaw deriving (Eq,Show,Read,Ord,Generic)
_RazorClaw :: HeldItem' RazorClaw
_RazorClaw = heldItem

data RindoBerry = RindoBerry deriving (Eq,Show,Read,Ord,Generic)
_RindoBerry :: HeldItem' RindoBerry
_RindoBerry = heldItem

data RockyHelmet = RockyHelmet deriving (Eq,Show,Read,Ord,Generic)
_RockyHelmet :: HeldItem' RockyHelmet
_RockyHelmet = heldItem

data ScopeLens = ScopeLens deriving (Eq,Show,Read,Ord,Generic)
_ScopeLens :: HeldItem' ScopeLens
_ScopeLens = heldItem

data SitrusBerry = SitrusBerry deriving (Eq,Show,Read,Ord,Generic)
_SitrusBerry :: HeldItem' SitrusBerry
_SitrusBerry = heldItem

data WhiteHerb = WhiteHerb deriving (Eq,Show,Read,Ord,Generic)
_WhiteHerb :: HeldItem' WhiteHerb
_WhiteHerb = heldItem

data ChilanBerry = ChilanBerry deriving (Eq,Show,Read,Ord,Generic)
_ChilanBerry :: HeldItem' ChilanBerry
_ChilanBerry = heldItem

data ColburBerry = ColburBerry deriving (Eq,Show,Read,Ord,Generic)
_ColburBerry :: HeldItem' ColburBerry
_ColburBerry = heldItem

data HabanBerry = HabanBerry deriving (Eq,Show,Read,Ord,Generic)
_HabanBerry :: HeldItem' HabanBerry
_HabanBerry = heldItem

data WacanBerry = WacanBerry deriving (Eq,Show,Read,Ord,Generic)
_WacanBerry :: HeldItem' WacanBerry
_WacanBerry = heldItem

data RoseliBerry = RoseliBerry deriving (Eq,Show,Read,Ord,Generic)
_RoseliBerry :: HeldItem' RoseliBerry
_RoseliBerry = heldItem

data ChopleBerry = ChopleBerry deriving (Eq,Show,Read,Ord,Generic)
_ChopleBerry :: HeldItem' ChopleBerry
_ChopleBerry = heldItem

data CobaBerry = CobaBerry deriving (Eq,Show,Read,Ord,Generic)
_CobaBerry :: HeldItem' CobaBerry
_CobaBerry = heldItem

data KasibBerry = KasibBerry deriving (Eq,Show,Read,Ord,Generic)
_KasibBerry :: HeldItem' KasibBerry
_KasibBerry = heldItem

data ShucaBerry = ShucaBerry deriving (Eq,Show,Read,Ord,Generic)
_ShucaBerry :: HeldItem' ShucaBerry
_ShucaBerry = heldItem

data YacheBerry = YacheBerry deriving (Eq,Show,Read,Ord,Generic)
_YacheBerry :: HeldItem' YacheBerry
_YacheBerry = heldItem

data KebiaBerry = KebiaBerry deriving (Eq,Show,Read,Ord,Generic)
_KebiaBerry :: HeldItem' KebiaBerry
_KebiaBerry = heldItem

data BabiriBerry = BabiriBerry deriving (Eq,Show,Read,Ord,Generic)
_BabiriBerry :: HeldItem' BabiriBerry
_BabiriBerry = heldItem

data PasshoBerry = PasshoBerry deriving (Eq,Show,Read,Ord,Generic)
_PasshoBerry :: HeldItem' PasshoBerry
_PasshoBerry = heldItem

data TangaBerry = TangaBerry deriving (Eq,Show,Read,Ord,Generic)
_TangaBerry :: HeldItem' TangaBerry
_TangaBerry = heldItem

data PayapaBerry = PayapaBerry deriving (Eq,Show,Read,Ord,Generic)
_PayapaBerry :: HeldItem' PayapaBerry
_PayapaBerry = heldItem


heldItemPrism :: Prism' HeldItem String
heldItemPrism = prism' HeldItem (\x -> Just $ heldItemToString x) 

battleItemPrism :: Prism' BattleItem String
battleItemPrism = prism' BattleItem (\x -> Just $ battleItemToString x) 

healingItemPrism :: Prism' HealingItem String
healingItemPrism = prism' HealingItem (\x -> Just $ healingItemToString x) 

heldItem :: (Read it, Show it) => Prism' HeldItem it
heldItem = heldItemPrism . mkPrism
