{-# LANGUAGE DeriveGeneric #-}


module Domain.Entity.BuiltIn.Pokemon where

import GHC.Generics
import Domain.Attribute.Quadruple
import Domain.Attribute.HeldItem
import Domain.Entity.BuiltIn.Pokemon.Species
import Domain.Entity.BuiltIn.Move
import Domain.Entity.Stats.Pokemon

data Red'sPokemon =
  Red'sPikachu Pikachu
  | Red'sLapras Lapras
  | Red'sSnorlax Snorlax
  | Red'sVenusaur Venusaur
  | Red'sCharizard Charizard
  | Red'sBlastoise Blastoise
  deriving (Eq,Show,Read,Generic)

data Blue'sPokemon =
  Blue'sAerodactyl Aerodactyl
  | Blue'sMachamp Machamp
  | Blue'sAlakazam Alakazam
  | Blue'sExeggutor Exeggutor
  | Blue'sArcanine Arcanine
  | Blue'sGyarados Gyarados
  deriving (Eq,Show,Read,Generic)

data Lance'sPokemon =
  Lance'sDragonite Dragonite
  | Lance'sSalamence Salamence
  | Lance'sKingdra Kingdra
  | Lance'sHaxorus Haxorus
  | Lance'sHydreigon Hydreigon
  | Lance'sFlygon Flygon
  deriving (Eq,Show,Read,Generic)

data Steven'sPokemon =
  Steven'sMetagross Metagross
  | Steven'sAggron Aggron
  | Steven'sExcadrill Excadrill
  | Steven'sArcheops Archeops
  | Steven'sCradily Cradily
  | Steven'sArmaldo Armaldo
  deriving (Eq,Show,Read,Generic)

data Wallace'sPokemon =
  Wallace'sMilotic Milotic
  | Wallace'sSharpedo Sharpedo
  | Wallace'sWalrein Walrein
  | Wallace'sLudicolo Ludicolo
  | Wallace'sSwampert Swampert
  | Wallace'sStarmie Starmie
  deriving (Eq,Show,Read,Generic)

data Cynthia'sPokemon =
  Cynthia'sGarchomp Garchomp
  | Cynthia'sSpiritomb Spiritomb
  | Cynthia'sRoserade Roserade
  | Cynthia'sTogekiss Togekiss
  | Cynthia'sLucario Lucario
  | Cynthia'sGlaceon Glaceon
  deriving (Eq,Show,Read,Generic)

data Alder'sPokemon =
  Alder'sVolcarona Volcarona
  | Alder'sConkeldurr Conkeldurr
  | Alder'sReuniclus Reuniclus
  | Alder'sKrookodile Krookodile
  | Alder'sChandelure Chandelure
  | Alder'sBraviary Braviary
  deriving (Eq,Show,Read,Generic)


--- Type class instances

instance PokemonIndividual Red'sPokemon where
  moves (Red'sPikachu _)      = mkMoves VoltTackle IronTail FakeOut BrickBreak
  moves (Red'sLapras _)       = mkMoves IceBeam HydroPump IceShard Thunderbolt
  moves (Red'sSnorlax _)      = mkMoves BodySlam Earthquake Crunch SeedBomb
  moves (Red'sVenusaur _)     = mkMoves LeafStorm SludgeBomb Earthquake SleepPowder
  moves (Red'sCharizard _)    = mkMoves FireBlast FocusBlast AirSlash DragonPulse
  moves (Red'sBlastoise _)    = mkMoves WaterSpout HydroPump Blizzard FocusBlast
  heldItem (Red'sPikachu _)   = LightBall
  heldItem (Red'sLapras _)    = SitrusBerry
  heldItem (Red'sSnorlax _)   = QuickClaw
  heldItem (Red'sVenusaur _)  = WhiteHerb
  heldItem (Red'sCharizard _) = FocusSash
  heldItem (Red'sBlastoise _) = ChoiceScarf

instance PokemonIndividual Blue'sPokemon where
  moves (Blue'sAerodactyl _)    = mkMoves StoneEdge Earthquake IceFang FireBlast
  moves (Blue'sMachamp _)       = mkMoves Superpower StoneEdge FirePunch BulletPunch
  moves (Blue'sAlakazam _)      = mkMoves Psychic FocusBlast ShadowBall Reflect
  moves (Blue'sExeggutor _)     = mkMoves LeafStorm WoodHammer ZenHeadbutt LeechSeed
  moves (Blue'sArcanine _)      = mkMoves FlareBlitz CloseCombat WildCharge ExtremeSpeed
  moves (Blue'sGyarados _)      = mkMoves Waterfall Earthquake IceFang Outrage
  heldItem (Blue'sAerodactyl _) = ChoiceBand
  heldItem (Blue'sMachamp _)    = LifeOrb
  heldItem (Blue'sAlakazam _)   = KingsRock
  heldItem (Blue'sExeggutor _)  = FocusSash
  heldItem (Blue'sArcanine _)   = ExpertBelt
  heldItem (Blue'sGyarados _)   = WhiteHerb
                                
instance PokemonIndividual Lance'sPokemon where
  moves (Lance'sDragonite _)    = mkMoves ExtremeSpeed IcePunch FirePunch DracoMeteor
  moves (Lance'sSalamence _)    = mkMoves DragonClaw Earthquake StoneEdge Crunch
  moves (Lance'sKingdra _)      = mkMoves Surf DragonPulse IceBeam FlashCannon
  moves (Lance'sHaxorus _)      = mkMoves Outrage Superpower Earthquake RockSlide
  moves (Lance'sHydreigon _)    = mkMoves DracoMeteor DarkPulse FireBlast EarthPower
  moves (Lance'sFlygon _)       = mkMoves DracoMeteor EarthPower SolarBeam UTurn
  heldItem (Lance'sDragonite _) = FocusSash
  heldItem (Lance'sSalamence _) = ExpertBelt
  heldItem (Lance'sKingdra _)   = ScopeLens
  heldItem (Lance'sHaxorus _)   = ChoiceScarf
  heldItem (Lance'sHydreigon _) = WhiteHerb
  heldItem (Lance'sFlygon _)    = PowerHerb

instance PokemonIndividual Steven'sPokemon where
  moves (Steven'sMetagross _)     = mkMoves ZenHeadbutt HammerArm Earthquake BulletPunch
  moves (Steven'sAggron _)        = mkMoves HeadSmash Avalanche Earthquake MetalBurst
  moves (Steven'sExcadrill _)     = mkMoves Earthquake RockSlide XScissor Sandstorm
  moves (Steven'sArcheops _)      = mkMoves HeadSmash Acrobatics Earthquake QuickAttack
  moves (Steven'sCradily _)       = mkMoves StoneEdge SeedBomb Earthquake Sandstorm
  moves (Steven'sArmaldo _)       = mkMoves XScissor RockBlast Earthquake Superpower
  heldItem (Steven'sMetagross _)  = OccaBerry
  heldItem (Steven'sAggron _)     = AirBalloon
  heldItem (Steven'sExcadrill _)  = FocusSash
  heldItem (Steven'sArcheops _)   = SitrusBerry
  heldItem (Steven'sCradily _)    = ExpertBelt
  heldItem (Steven'sArmaldo _)    = WhiteHerb

instance PokemonIndividual Wallace'sPokemon where
  moves (Wallace'sMilotic _)      = mkMoves Scald IcyWind Rest SleepTalk
  moves (Wallace'sSharpedo _)     = mkMoves HydroPump Crunch ZenHeadbutt AquaJet
  moves (Wallace'sWalrein _)      = mkMoves Surf Yawn Blizzard SheerCold
  moves (Wallace'sLudicolo _)     = mkMoves Surf FocusBlast GigaDrain RainDance
  moves (Wallace'sSwampert _)     = mkMoves MuddyWater EarthPower IceBeam FocusBlast
  moves (Wallace'sStarmie _)      = mkMoves Surf Psychic Thunderbolt SignalBeam
  heldItem (Wallace'sMilotic _)   = RockyHelmet
  heldItem (Wallace'sSharpedo _)  = FocusSash
  heldItem (Wallace'sWalrein _)   = Leftovers
  heldItem (Wallace'sLudicolo _)  = LifeOrb
  heldItem (Wallace'sSwampert _)  = RindoBerry
  heldItem (Wallace'sStarmie _)   = ExpertBelt
  

instance PokemonIndividual Cynthia'sPokemon where
  moves (Cynthia'sGarchomp _)     = mkMoves Outrage Earthquake StoneEdge SwordsDance
  moves (Cynthia'sSpiritomb _)    = mkMoves SuckerPunch Protect WillOWisp PainSplit
  moves (Cynthia'sRoserade _)     = mkMoves LeafStorm SludgeBomb ShadowBall SleepPowder
  moves (Cynthia'sTogekiss _)     = mkMoves AirSlash AuraSphere ShadowBall GrassKnot
  moves (Cynthia'sLucario _)      = mkMoves CloseCombat DarkPulse StoneEdge ExtremeSpeed
  moves (Cynthia'sGlaceon _)      = mkMoves IceBeam ShadowBall SignalBeam WaterPulse
  heldItem (Cynthia'sGarchomp _)  = FocusSash
  heldItem (Cynthia'sSpiritomb _) = RockyHelmet
  heldItem (Cynthia'sRoserade _)  = WhiteHerb
  heldItem (Cynthia'sTogekiss _)  = Leftovers
  heldItem (Cynthia'sLucario _)   = LifeOrb
  heldItem (Cynthia'sGlaceon _)   = ChoiceScarf

   
instance PokemonIndividual Alder'sPokemon where
  moves (Alder'sVolcarona _)     = mkMoves HeatWave BugBuzz Psychic QuiverDance
  moves (Alder'sConkeldurr _)    = mkMoves HammerArm Payback StoneEdge MachPunch
  moves (Alder'sReuniclus _)     = mkMoves Psychic Reflect Toxic LightScreen
  moves (Alder'sKrookodile _)    = mkMoves Earthquake Crunch StoneEdge Outrage
  moves (Alder'sChandelure _)    = mkMoves Flamethrower ShadowBall Psychic EnergyBall
  moves (Alder'sBraviary _)      = mkMoves BraveBird Superpower RockSlide UTurn
  heldItem (Alder'sVolcarona _)  = ChartiBerry
  heldItem (Alder'sConkeldurr _) = LifeOrb
  heldItem (Alder'sReuniclus _)  = Leftovers
  heldItem (Alder'sKrookodile _) = ExpertBelt
  heldItem (Alder'sChandelure _) = ChoiceScarf
  heldItem (Alder'sBraviary _)   = ChoiceBand

-- Basic instances

instance PokemonStat Red'sPokemon where
  baseStat s pkmn = case pkmn of
    Red'sPikachu p   -> baseStat s p
    Red'sLapras p    -> baseStat s p
    Red'sSnorlax p   -> baseStat s p
    Red'sVenusaur p  -> baseStat s p
    Red'sCharizard p -> baseStat s p
    Red'sBlastoise p -> baseStat s p

instance PokemonAttribute Red'sPokemon where
  abilityIs pkmn = case pkmn of
    Red'sPikachu p   -> abilityIs p
    Red'sLapras p    -> abilityIs p
    Red'sSnorlax p   -> abilityIs p
    Red'sVenusaur p  -> abilityIs p
    Red'sCharizard p -> abilityIs p
    Red'sBlastoise p -> abilityIs p
  typeIs pkmn = case pkmn of
    Red'sPikachu p   -> typeIs p
    Red'sLapras p    -> typeIs p
    Red'sSnorlax p   -> typeIs p
    Red'sVenusaur p  -> typeIs p
    Red'sCharizard p -> typeIs p
    Red'sBlastoise p -> typeIs p
  genderIs pkmn = case pkmn of
    Red'sPikachu p   -> genderIs p
    Red'sLapras p    -> genderIs p
    Red'sSnorlax p   -> genderIs p
    Red'sVenusaur p  -> genderIs p
    Red'sCharizard p -> genderIs p
    Red'sBlastoise p -> genderIs p


instance PokemonStat Blue'sPokemon where
  baseStat s pkmn = case pkmn of
    Blue'sAerodactyl p -> baseStat s p
    Blue'sMachamp p    -> baseStat s p
    Blue'sAlakazam p   -> baseStat s p
    Blue'sExeggutor p  -> baseStat s p
    Blue'sArcanine p   -> baseStat s p
    Blue'sGyarados p   -> baseStat s p

instance PokemonAttribute Blue'sPokemon where
  abilityIs pkmn = case pkmn of
    Blue'sAerodactyl p -> abilityIs p
    Blue'sMachamp p    -> abilityIs p
    Blue'sAlakazam p   -> abilityIs p
    Blue'sExeggutor p  -> abilityIs p
    Blue'sArcanine p   -> abilityIs p
    Blue'sGyarados p   -> abilityIs p
  typeIs pkmn = case pkmn of
    Blue'sAerodactyl p -> typeIs p
    Blue'sMachamp p    -> typeIs p
    Blue'sAlakazam p   -> typeIs p
    Blue'sExeggutor p  -> typeIs p
    Blue'sArcanine p   -> typeIs p
    Blue'sGyarados p   -> typeIs p
  genderIs pkmn = case pkmn of
    Blue'sAerodactyl p -> genderIs p
    Blue'sMachamp p    -> genderIs p
    Blue'sAlakazam p   -> genderIs p
    Blue'sExeggutor p  -> genderIs p
    Blue'sArcanine p   -> genderIs p
    Blue'sGyarados p   -> genderIs p

  

instance PokemonStat Lance'sPokemon where
  baseStat s pkmn = case pkmn of
    Lance'sDragonite p -> baseStat s p
    Lance'sSalamence p -> baseStat s p
    Lance'sKingdra p -> baseStat s p
    Lance'sHaxorus p -> baseStat s p
    Lance'sHydreigon p -> baseStat s p
    Lance'sFlygon p -> baseStat s p

instance PokemonAttribute Lance'sPokemon where
  abilityIs pkmn = case pkmn of
    Lance'sDragonite p -> abilityIs p
    Lance'sSalamence p -> abilityIs p
    Lance'sKingdra p -> abilityIs p
    Lance'sHaxorus p -> abilityIs p
    Lance'sHydreigon p -> abilityIs p
    Lance'sFlygon p -> abilityIs p
  typeIs pkmn = case pkmn of
    Lance'sDragonite p -> typeIs p
    Lance'sSalamence p -> typeIs p
    Lance'sKingdra p -> typeIs p
    Lance'sHaxorus p -> typeIs p
    Lance'sHydreigon p -> typeIs p
    Lance'sFlygon p -> typeIs p
  genderIs pkmn = case pkmn of
    Lance'sDragonite p -> genderIs p
    Lance'sSalamence p -> genderIs p
    Lance'sKingdra p -> genderIs p
    Lance'sHaxorus p -> genderIs p
    Lance'sHydreigon p -> genderIs p
    Lance'sFlygon p -> genderIs p


instance PokemonStat Steven'sPokemon where
  baseStat s pkmn = case pkmn of
    Steven'sMetagross p -> baseStat s p
    Steven'sAggron p -> baseStat s p
    Steven'sExcadrill p -> baseStat s p
    Steven'sArcheops p -> baseStat s p
    Steven'sCradily p -> baseStat s p
    Steven'sArmaldo p -> baseStat s p

instance PokemonAttribute Steven'sPokemon where
  abilityIs pkmn = case pkmn of
    Steven'sMetagross p -> abilityIs p
    Steven'sAggron p -> abilityIs p
    Steven'sExcadrill p -> abilityIs p
    Steven'sArcheops p -> abilityIs p
    Steven'sCradily p -> abilityIs p
    Steven'sArmaldo p -> abilityIs p
  typeIs pkmn = case pkmn of
    Steven'sMetagross p -> typeIs p
    Steven'sAggron p -> typeIs p
    Steven'sExcadrill p -> typeIs p
    Steven'sArcheops p -> typeIs p
    Steven'sCradily p -> typeIs p
    Steven'sArmaldo p -> typeIs p
  genderIs pkmn = case pkmn of
    Steven'sMetagross p -> genderIs p
    Steven'sAggron p -> genderIs p
    Steven'sExcadrill p -> genderIs p
    Steven'sArcheops p -> genderIs p
    Steven'sCradily p -> genderIs p
    Steven'sArmaldo p -> genderIs p


instance PokemonStat Wallace'sPokemon where
  baseStat s pkmn = case pkmn of
    Wallace'sMilotic p -> baseStat s p
    Wallace'sSharpedo p -> baseStat s p
    Wallace'sWalrein p -> baseStat s p
    Wallace'sLudicolo p -> baseStat s p
    Wallace'sSwampert p -> baseStat s p
    Wallace'sStarmie p -> baseStat s p

instance PokemonAttribute Wallace'sPokemon where
  abilityIs pkmn = case pkmn of
    Wallace'sMilotic p -> abilityIs p
    Wallace'sSharpedo p -> abilityIs p
    Wallace'sWalrein p -> abilityIs p
    Wallace'sLudicolo p -> abilityIs p
    Wallace'sSwampert p -> abilityIs p
    Wallace'sStarmie p -> abilityIs p
  typeIs pkmn = case pkmn of
    Wallace'sMilotic p -> typeIs p
    Wallace'sSharpedo p -> typeIs p
    Wallace'sWalrein p -> typeIs p
    Wallace'sLudicolo p -> typeIs p
    Wallace'sSwampert p -> typeIs p
    Wallace'sStarmie p -> typeIs p
  genderIs pkmn = case pkmn of
    Wallace'sMilotic p -> genderIs p
    Wallace'sSharpedo p -> genderIs p
    Wallace'sWalrein p -> genderIs p
    Wallace'sLudicolo p -> genderIs p
    Wallace'sSwampert p -> genderIs p
    Wallace'sStarmie p -> genderIs p


instance PokemonStat Cynthia'sPokemon where
  baseStat s pkmn = case pkmn of
    Cynthia'sGarchomp p -> baseStat s p
    Cynthia'sSpiritomb p -> baseStat s p
    Cynthia'sRoserade p -> baseStat s p
    Cynthia'sTogekiss p -> baseStat s p
    Cynthia'sLucario p -> baseStat s p
    Cynthia'sGlaceon p -> baseStat s p

instance PokemonAttribute Cynthia'sPokemon where
  abilityIs pkmn = case pkmn of
    Cynthia'sGarchomp p -> abilityIs p
    Cynthia'sSpiritomb p -> abilityIs p
    Cynthia'sRoserade p -> abilityIs p
    Cynthia'sTogekiss p -> abilityIs p
    Cynthia'sLucario p -> abilityIs p
    Cynthia'sGlaceon p -> abilityIs p
  typeIs pkmn = case pkmn of
    Cynthia'sGarchomp p -> typeIs p
    Cynthia'sSpiritomb p -> typeIs p
    Cynthia'sRoserade p -> typeIs p
    Cynthia'sTogekiss p -> typeIs p
    Cynthia'sLucario p -> typeIs p
    Cynthia'sGlaceon p -> typeIs p
  genderIs pkmn = case pkmn of
    Cynthia'sGarchomp p -> genderIs p
    Cynthia'sSpiritomb p -> genderIs p
    Cynthia'sRoserade p -> genderIs p
    Cynthia'sTogekiss p -> genderIs p
    Cynthia'sLucario p -> genderIs p
    Cynthia'sGlaceon p -> genderIs p


instance PokemonStat Alder'sPokemon where
  baseStat s pkmn = case pkmn of
    Alder'sVolcarona p -> baseStat s p
    Alder'sConkeldurr p -> baseStat s p
    Alder'sReuniclus p -> baseStat s p
    Alder'sKrookodile p -> baseStat s p
    Alder'sChandelure p -> baseStat s p
    Alder'sBraviary p -> baseStat s p

instance PokemonAttribute Alder'sPokemon where
  abilityIs pkmn = case pkmn of
    Alder'sVolcarona p -> abilityIs p
    Alder'sConkeldurr p -> abilityIs p
    Alder'sReuniclus p -> abilityIs p
    Alder'sKrookodile p -> abilityIs p
    Alder'sChandelure p -> abilityIs p
    Alder'sBraviary p -> abilityIs p
  typeIs pkmn = case pkmn of
    Alder'sVolcarona p -> typeIs p
    Alder'sConkeldurr p -> typeIs p
    Alder'sReuniclus p -> typeIs p
    Alder'sKrookodile p -> typeIs p
    Alder'sChandelure p -> typeIs p
    Alder'sBraviary p -> typeIs p
  genderIs pkmn = case pkmn of
    Alder'sVolcarona p -> genderIs p
    Alder'sConkeldurr p -> genderIs p
    Alder'sReuniclus p -> genderIs p
    Alder'sKrookodile p -> genderIs p
    Alder'sChandelure p -> genderIs p
    Alder'sBraviary p -> genderIs p



-- Helper


mkMoves ::
  (MakeMove m1, MakeMove m2, MakeMove m3, MakeMove m4)
  => m1 -> m2 -> m3 -> m4 -> Quadruple Move
mkMoves m1 m2 m3 m4 = Quadruple (mkMove m1) (mkMove m2) (mkMove m3) (mkMove m4)
