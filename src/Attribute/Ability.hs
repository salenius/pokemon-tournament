{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}


module Attribute.Ability  where

import Attribute.Env
import Prelude hiding (filter)
import GHC.Generics

newtype PokemonAbility = PokemonAbility {abilityToString :: String} deriving (Eq,Show,Read,Ord,Generic)

type Ability a = Prism' PokemonAbility a

data MoldBreaker_ =
  MoldBreaker_ MoldBreaker
  | Turboblaze_ Turboblaze
  | Teravolt_ Teravolt
  deriving (Eq,Show,Read,Ord,Generic)

data BattleArmor_ =
  BattleArmor_ BattleArmor
  | ShellArmor_ ShellArmor
  deriving (Eq,Show,Read,Ord,Generic)

data Insomnia_ =
  Insomnia_ Insomnia
  | VitalSpirit_ VitalSpirit
  deriving (Eq,Show,Read,Ord,Generic)

data WaterVeil_ =
  WaterVeil_ WaterVeil
  | WaterBubble_ WaterBubble
  deriving (Eq,Show,Read,Ord,Generic)

data SnowCloak = SnowCloak deriving (Eq,Show,Read,Ord,Generic)
_SnowCloak :: Ability SnowCloak
_SnowCloak = ability

data MoldBreaker = MoldBreaker deriving (Eq,Show,Read,Ord,Generic)
_MoldBreaker :: Ability MoldBreaker
_MoldBreaker = ability

data Teravolt = Teravolt deriving (Eq,Show,Read,Ord,Generic)
_Teravolt :: Ability Teravolt
_Teravolt = ability

data Turboblaze = Turboblaze deriving (Eq,Show,Read,Ord,Generic)
_Turboblaze :: Ability Turboblaze
_Turboblaze = ability

data Adaptability = Adaptability deriving (Eq,Show,Read,Ord,Generic)
_Adaptability :: Ability Adaptability
_Adaptability = ability

data Aerilate = Aerilate deriving (Eq,Show,Read,Ord,Generic)
_Aerilate :: Ability Aerilate
_Aerilate = ability

data BattleArmor = BattleArmor deriving (Eq,Show,Read,Ord,Generic)
_BattleArmor :: Ability BattleArmor
_BattleArmor = ability

data BigPecks = BigPecks deriving (Eq,Show,Read,Ord,Generic)
_BigPecks :: Ability BigPecks
_BigPecks = ability

data Blaze = Blaze deriving (Eq,Show,Read,Ord,Generic)
_Blaze :: Ability Blaze
_Blaze = ability

data Bulletproof = Bulletproof deriving (Eq,Show,Read,Ord,Generic)
_Bulletproof :: Ability Bulletproof
_Bulletproof = ability

data Chlorophyll = Chlorophyll deriving (Eq,Show,Read,Ord,Generic)
_Chlorophyll :: Ability Chlorophyll
_Chlorophyll = ability

data ClearBody = ClearBody deriving (Eq,Show,Read,Ord,Generic)
_ClearBody :: Ability ClearBody
_ClearBody = ability

data CloudNine = CloudNine deriving (Eq,Show,Read,Ord,Generic)
_CloudNine :: Ability CloudNine
_CloudNine = ability

data Comatose = Comatose deriving (Eq,Show,Read,Ord,Generic)
_Comatose :: Ability Comatose
_Comatose = ability

data Defeatist = Defeatist deriving (Eq,Show,Read,Ord,Generic)
_Defeatist :: Ability Defeatist
_Defeatist = ability

data Drought = Drought deriving (Eq,Show,Read,Ord,Generic)
_Drought :: Ability Drought
_Drought = ability

data FlameBody = FlameBody deriving (Eq,Show,Read,Ord,Generic)
_FlameBody :: Ability FlameBody
_FlameBody = ability

data FlashFire = FlashFire deriving (Eq,Show,Read,Ord,Generic)
_FlashFire :: Ability FlashFire
_FlashFire = ability

data FlowerVeil = FlowerVeil deriving (Eq,Show,Read,Ord,Generic)
_FlowerVeil :: Ability FlowerVeil
_FlowerVeil = ability

data Frisk = Frisk deriving (Eq,Show,Read,Ord,Generic)
_Frisk :: Ability Frisk
_Frisk = ability

data FullMetalBody = FullMetalBody deriving (Eq,Show,Read,Ord,Generic)
_FullMetalBody :: Ability FullMetalBody
_FullMetalBody = ability

data Guts = Guts deriving (Eq,Show,Read,Ord,Generic)
_Guts :: Ability Guts
_Guts = ability

data Hustle = Hustle deriving (Eq,Show,Read,Ord,Generic)
_Hustle :: Ability Hustle
_Hustle = ability

data HyperCutter = HyperCutter deriving (Eq,Show,Read,Ord,Generic)
_HyperCutter :: Ability HyperCutter
_HyperCutter = ability

data IceBody = IceBody deriving (Eq,Show,Read,Ord,Generic)
_IceBody :: Ability IceBody
_IceBody = ability

data Illuminate = Illuminate deriving (Eq,Show,Read,Ord,Generic)
_Illuminate :: Ability Illuminate
_Illuminate = ability

data Immunity = Immunity deriving (Eq,Show,Read,Ord,Generic)
_Immunity :: Ability Immunity
_Immunity = ability

data InnerFocus = InnerFocus deriving (Eq,Show,Read,Ord,Generic)
_InnerFocus :: Ability InnerFocus
_InnerFocus = ability

data Insomnia = Insomnia deriving (Eq,Show,Read,Ord,Generic)
_Insomnia :: Ability Insomnia
_Insomnia = ability

data Intimidate = Intimidate deriving (Eq,Show,Read,Ord,Generic)
_Intimidate :: Ability Intimidate
_Intimidate = ability

data KeenEye = KeenEye deriving (Eq,Show,Read,Ord,Generic)
_KeenEye :: Ability KeenEye
_KeenEye = ability

data LeafGuard = LeafGuard deriving (Eq,Show,Read,Ord,Generic)
_LeafGuard :: Ability LeafGuard
_LeafGuard = ability

data Levitate = Levitate deriving (Eq,Show,Read,Ord,Generic)
_Levitate :: Ability Levitate
_Levitate = ability

data LiquidVoice = LiquidVoice deriving (Eq,Show,Read,Ord,Generic)
_LiquidVoice :: Ability LiquidVoice
_LiquidVoice = ability

data Limber = Limber deriving (Eq,Show,Read,Ord,Generic)
_Limber :: Ability Limber
_Limber = ability

data MagicBounce = MagicBounce deriving (Eq,Show,Read,Ord,Generic)
_MagicBounce :: Ability MagicBounce
_MagicBounce = ability

data MagicGuard = MagicGuard deriving (Eq,Show,Read,Ord,Generic)
_MagicGuard :: Ability MagicGuard
_MagicGuard = ability

data MagmaArmor = MagmaArmor deriving (Eq,Show,Read,Ord,Generic)
_MagmaArmor :: Ability MagmaArmor
_MagmaArmor = ability

data MarvelScale = MarvelScale deriving (Eq,Show,Read,Ord,Generic)
_MarvelScale :: Ability MarvelScale
_MarvelScale = ability

data MegaLauncher = MegaLauncher deriving (Eq,Show,Read,Ord,Generic)
_MegaLauncher :: Ability MegaLauncher
_MegaLauncher = ability

data Merciless = Merciless deriving (Eq,Show,Read,Ord,Generic)
_Merciless :: Ability Merciless
_Merciless = ability

data Moxie = Moxie deriving (Eq,Show,Read,Ord,Generic)
_Moxie :: Ability Moxie
_Moxie = ability

data NaturalCure = NaturalCure deriving (Eq,Show,Read,Ord,Generic)
_NaturalCure :: Ability NaturalCure
_NaturalCure = ability

data Neuroforce = Neuroforce deriving (Eq,Show,Read,Ord,Generic)
_Neuroforce :: Ability Neuroforce
_Neuroforce = ability

data NoGuard = NoGuard deriving (Eq,Show,Read,Ord,Generic)
_NoGuard :: Ability NoGuard
_NoGuard = ability

data Normalize = Normalize deriving (Eq,Show,Read,Ord,Generic)
_Normalize :: Ability Normalize
_Normalize = ability

data Oblivious = Oblivious deriving (Eq,Show,Read,Ord,Generic)
_Oblivious :: Ability Oblivious
_Oblivious = ability

data Overcoat = Overcoat deriving (Eq,Show,Read,Ord,Generic)
_Overcoat :: Ability Overcoat
_Overcoat = ability

data Overgrow = Overgrow deriving (Eq,Show,Read,Ord,Generic)
_Overgrow :: Ability Overgrow
_Overgrow = ability

data PoisonPoint = PoisonPoint deriving (Eq,Show,Read,Ord,Generic)
_PoisonPoint :: Ability PoisonPoint
_PoisonPoint = ability

data Pressure = Pressure deriving (Eq,Show,Read,Ord,Generic)
_Pressure :: Ability Pressure
_Pressure = ability

data RainDish = RainDish deriving (Eq,Show,Read,Ord,Generic)
_RainDish :: Ability RainDish
_RainDish = ability

data Regenerator = Regenerator deriving (Eq,Show,Read,Ord,Generic)
_Regenerator :: Ability Regenerator
_Regenerator = ability

data Rivalry = Rivalry deriving (Eq,Show,Read,Ord,Generic)
_Rivalry :: Ability Rivalry
_Rivalry = ability

data RockHead = RockHead deriving (Eq,Show,Read,Ord,Generic)
_RockHead :: Ability RockHead
_RockHead = ability

data RoughSkin = RoughSkin deriving (Eq,Show,Read,Ord,Generic)
_RoughSkin :: Ability RoughSkin
_RoughSkin = ability

data SandForce = SandForce deriving (Eq,Show,Read,Ord,Generic)
_SandForce :: Ability SandForce
_SandForce = ability

data SandRush = SandRush deriving (Eq,Show,Read,Ord,Generic)
_SandRush :: Ability SandRush
_SandRush = ability

data SandVeil = SandVeil deriving (Eq,Show,Read,Ord,Generic)
_SandVeil :: Ability SandVeil
_SandVeil = ability

data SapSipper = SapSipper deriving (Eq,Show,Read,Ord,Generic)
_SapSipper :: Ability SapSipper
_SapSipper = ability

data SereneGrace = SereneGrace deriving (Eq,Show,Read,Ord,Generic)
_SereneGrace :: Ability SereneGrace
_SereneGrace = ability

data SheerForce = SheerForce deriving (Eq,Show,Read,Ord,Generic)
_SheerForce :: Ability SheerForce
_SheerForce = ability

data ShellArmor = ShellArmor deriving (Eq,Show,Read,Ord,Generic)
_ShellArmor :: Ability ShellArmor
_ShellArmor = ability

data Sniper = Sniper deriving (Eq,Show,Read,Ord,Generic)
_Sniper :: Ability Sniper
_Sniper = ability

data SolidRock = SolidRock deriving (Eq,Show,Read,Ord,Generic)
_SolidRock :: Ability SolidRock
_SolidRock = ability

data Filter = Filter deriving (Eq,Show,Read,Ord,Generic)
_Filter :: Ability Filter
_Filter = ability

data PrismArmor = PrismArmor deriving (Eq,Show,Read,Ord,Generic)
_PrismArmor :: Ability PrismArmor
_PrismArmor = ability

data Soundproof = Soundproof deriving (Eq,Show,Read,Ord,Generic)
_Soundproof :: Ability Soundproof
_Soundproof = ability

data Static = Static deriving (Eq,Show,Read,Ord,Generic)
_Static :: Ability Static
_Static = ability

data Steadfast = Steadfast deriving (Eq,Show,Read,Ord,Generic)
_Steadfast :: Ability Steadfast
_Steadfast = ability

data StormDrain = StormDrain deriving (Eq,Show,Read,Ord,Generic)
_StormDrain :: Ability StormDrain
_StormDrain = ability

data StrongJaw = StrongJaw deriving (Eq,Show,Read,Ord,Generic)
_StrongJaw :: Ability StrongJaw
_StrongJaw = ability

data Sturdy = Sturdy deriving (Eq,Show,Read,Ord,Generic)
_Sturdy :: Ability Sturdy
_Sturdy = ability

data SuctionCups = SuctionCups deriving (Eq,Show,Read,Ord,Generic)
_SuctionCups :: Ability SuctionCups
_SuctionCups = ability

data SwiftSwim = SwiftSwim deriving (Eq,Show,Read,Ord,Generic)
_SwiftSwim :: Ability SwiftSwim
_SwiftSwim = ability

data Synchronize = Synchronize deriving (Eq,Show,Read,Ord,Generic)
_Synchronize :: Ability Synchronize
_Synchronize = ability

data ThickFat = ThickFat deriving (Eq,Show,Read,Ord,Generic)
_ThickFat :: Ability ThickFat
_ThickFat = ability

data ToughClaws = ToughClaws deriving (Eq,Show,Read,Ord,Generic)
_ToughClaws :: Ability ToughClaws
_ToughClaws = ability

data Torrent = Torrent deriving (Eq,Show,Read,Ord,Generic)
_Torrent :: Ability Torrent
_Torrent = ability

data VitalSpirit = VitalSpirit deriving (Eq,Show,Read,Ord,Generic)
_VitalSpirit :: Ability VitalSpirit
_VitalSpirit = ability

data VoltAbsorb = VoltAbsorb deriving (Eq,Show,Read,Ord,Generic)
_VoltAbsorb :: Ability VoltAbsorb
_VoltAbsorb = ability

data WaterAbsorb = WaterAbsorb deriving (Eq,Show,Read,Ord,Generic)
_WaterAbsorb :: Ability WaterAbsorb
_WaterAbsorb = ability

data WaterBubble = WaterBubble deriving (Eq,Show,Read,Ord,Generic)
_WaterBubble :: Ability WaterBubble
_WaterBubble = ability

data WaterVeil = WaterVeil deriving (Eq,Show,Read,Ord,Generic)
_WaterVeil :: Ability WaterVeil
_WaterVeil = ability

data WhiteSmoke = WhiteSmoke deriving (Eq,Show,Read,Ord,Generic)
_WhiteSmoke :: Ability WhiteSmoke
_WhiteSmoke = ability

-----------

abPrism :: Prism' PokemonAbility String
abPrism = prism' PokemonAbility (\x -> Just $ abilityToString x) 

ability :: (Read a, Show a) => Prism' PokemonAbility a
ability = abPrism . mkPrism

_MoldBreaker_ :: Ability MoldBreaker_
_MoldBreaker_ = prism' f g
  where
    f (MoldBreaker_ x) = PokemonAbility . show $ x
    f (Teravolt_ x) = PokemonAbility . show $ x
    f (Turboblaze_ x) = PokemonAbility . show $ x
    g ab = case abilityToString ab of
      "MoldBreaker" -> Just (MoldBreaker_ MoldBreaker)
      "Teravolt" -> Just (Teravolt_ Teravolt)
      "Turboblaze" -> Just (Turboblaze_ Turboblaze)
      _ -> Nothing

notMoldBreaker :: PokemonAbility -> Maybe ()
notMoldBreaker = (\ab -> case ab of
                     Just _ -> Nothing
                     Nothing -> Just ()) . preview _MoldBreaker_
