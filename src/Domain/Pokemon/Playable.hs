module Domain.Pokemon.Playable (
  getParty
                               ) where

import Domain.Entity.Trainer
import Domain.Pokemon.Individual
import Domain.Pokemon.BuiltIn
import Domain.Move.BuiltIn
import Domain.Attribute.HeldItem

getParty :: Trainer BuiltInPokemon BuiltInMove -> Party (Pokemon BuiltInPokemon BuiltInMove)
getParty Red = PartyOf6 p1 p2 p3 p4 p5 p6
  where
    p1 = buildPkmn Pikachu LightBall (VoltTackle, IronTail, FakeOut, BrickBreak)
    p2 = buildPkmn Lapras SitrusBerry (IceBeam, HydroPump, IceShard, Thunderbolt)
    p3 = buildPkmn Snorlax QuickClaw (BodySlam, Earthquake, Crunch, SeedBomb)
    p4 = buildPkmn Venusaur WhiteHerb (LeafStorm, SludgeBomb, Earthquake, SleepPowder)
    p5 = buildPkmn Charizard FocusSash (FireBlast, FocusBlast, AirSlash, DragonPulse)
    p6 = buildPkmn Blastoise ChoiceScarf (WaterSpout, HydroPump, Blizzard, FocusBlast)
getParty Blue = PartyOf6 p1 p2 p3 p4 p5 p6
  where
    p1 = buildPkmn Aerodactyl ChoiceBand (StoneEdge, Earthquake, IceFang, FireBlast)
    p2 = buildPkmn Machamp WhiteHerb (Superpower, StoneEdge, FirePunch, BulletPunch)
    p3 = buildPkmn Alakazam FocusSash (Psychic, FocusBlast, ShadowBall, Reflect)
    p4 = buildPkmn Exeggutor LifeOrb (LeafStorm, WoodHammer, ZenHeadbutt, LeechSeed)
    p5 = buildPkmn Arcanine ExpertBelt (FlareBlitz, CloseCombat, WildCharge, ExtremeSpeed)
    p6 = buildPkmn Gyarados KingsRock (Waterfall, Earthquake, IceFang, Outrage)
getParty Lance = PartyOf6 p1 p2 p3 p4 p5 p6
  where
    p1 = buildPkmn Dragonite FocusSash (ExtremeSpeed, IcePunch, FirePunch, DracoMeteor)
    p2 = buildPkmn Salamence ExpertBelt (DragonClaw, Earthquake, StoneEdge, Crunch)
    p3 = buildPkmn Kingdra ScopeLens (Surf, DragonPulse, IceBeam, FlashCannon)
    p4 = buildPkmn Haxorus ChoiceScarf (Outrage, Superpower, Earthquake, RockSlide)
    p5 = buildPkmn Hydreigon WhiteHerb (DracoMeteor, DarkPulse, FireBlast, EarthPower)
    p6 = buildPkmn Flygon PowerHerb (DracoMeteor, EarthPower, SolarBeam, UTurn)
getParty Steven = PartyOf6 p1 p2 p3 p4 p5 p6
  where
    p1 = buildPkmn Metagross OccaBerry (ZenHeadbutt, HammerArm, Earthquake, BulletPunch)
    p2 = buildPkmn Aggron AirBalloon (HeadSmash, Avalanche, Earthquake, MetalBurst)
    p3 = buildPkmn Excadrill FocusSash (Earthquake, RockSlide, XScissor, Sandstorm)
    p4 = buildPkmn Archeops SitrusBerry (HeadSmash, Acrobatics, Earthquake, QuickAttack)
    p5 = buildPkmn Cradily ExpertBelt (StoneEdge, SeedBomb, Earthquake, Sandstorm)
    p6 = buildPkmn Armaldo WhiteHerb (XScissor, RockBlast, Earthquake, Superpower)
getParty Wallace = PartyOf6 p1 p2 p3 p4 p5 p6
  where
    p1 = buildPkmn Milotic RockyHelmet (Scald, IcyWind, Rest, SleepTalk)
    p2 = buildPkmn Sharpedo FocusSash (HydroPump, Crunch, ZenHeadbutt, AquaJet)
    p3 = buildPkmn Walrein Leftovers (Surf, Yawn, Blizzard, SheerCold)
    p4 = buildPkmn Ludicolo LifeOrb (Surf, FocusBlast, GigaDrain, RainDance)
    p5 = buildPkmn Swampert RindoBerry (MuddyWater, EarthPower, IceBeam, FocusBlast)
    p6 = buildPkmn Starmie ExpertBelt (Surf, Psychic, Thunderbolt, SignalBeam)
getParty Cynthia = PartyOf6 p1 p2 p3 p4 p5 p6
  where
    p1 = buildPkmn Garchomp FocusSash (Outrage, Earthquake, StoneEdge, SwordsDance)
    p2 = buildPkmn Spiritomb RockyHelmet (SuckerPunch, Protect, WillOWisp, PainSplit)
    p3 = buildPkmn Roserade WhiteHerb (LeafStorm, SludgeBomb, ShadowBall, SleepPowder)
    p4 = buildPkmn Togekiss Leftovers (AirSlash, AuraSphere, ShadowBall, GrassKnot)
    p5 = buildPkmn Lucario LifeOrb (CloseCombat, DarkPulse, StoneEdge, ExtremeSpeed)
    p6 = buildPkmn Glaceon ChoiceScarf (IceBeam, ShadowBall, SignalBeam, WaterPulse)
getParty Alder = PartyOf6 p1 p2 p3 p4 p5 p6
  where
    p1 = buildPkmn Volcarona ChartiBerry (HeatWave, BugBuzz, Psychic, QuiverDance)
    p2 = buildPkmn Conkeldurr LifeOrb (HammerArm, Payback, StoneEdge, MachPunch)
    p3 = buildPkmn Reuniclus Leftovers (Psychic, Reflect, Toxic, LightScreen)
    p4 = buildPkmn Krookodile ExpertBelt (Earthquake, Crunch, StoneEdge, Outrage)
    p5 = buildPkmn Chandelure ChoiceScarf (Flamethrower, ShadowBall, Psychic, EnergyBall)
    p6 = buildPkmn Braviary ChoiceBand (BraveBird, Superpower, RockSlide, UTurn)
getParty (Trainer trnr) = _customTrainerParty trnr
  

buildPkmn :: BuiltInPokemon -> HeldItem -> FourMoves -> Pokemon BuiltInPokemon BuiltInMove
buildPkmn pkmn item (m1,m2,m3,m4) =
  Pokemon pkmn (Just item) (Quadruple m1 m2 m3 m4) defaultFactors

type FourMoves = (BuiltInMove, BuiltInMove, BuiltInMove, BuiltInMove)
