module Domain.Data.Pokemon where

import Domain.Data.PokemonSpecies
import Domain.Attribute.Ability
import Domain.Attribute.HeldItem
import Domain.Entity.Trainer
import Domain.Entity.Pokemon
import Domain.Data.Move
import Domain.Entity.Move

red'sPikachu =
  mkPokemon pikachu [Static] LightBall voltTackle ironTail fakeOut brickBreak

red'sLapras =
  mkPokemon lapras [WaterAbsorb,ShellArmor] SitrusBerry iceBeam hydroPump iceShard thunderbolt

red'sSnorlax =
  mkPokemon snorlax [Immunity,ThickFat] QuickClaw bodySlam earthquake crunch seedBomb

red'sVenusaur =
  mkPokemon venusaur [Overgrow] WhiteHerb leafStorm sludgeBomb earthquake sleepPowder

red'sCharizard =
  mkPokemon charizard [Blaze] FocusSash fireBlast focusBlast airSlash dragonPulse

red'sBlastoise =
  mkPokemon blastoise [Torrent] ChoiceScarf waterSpout hydroPump blizzard focusBlast

blue'sAerodactyl =
  mkPokemon aerodactyl [RockHead,Pressure] ChoiceBand stoneEdge earthquake iceFang fireBlast

blue'sMachamp =
  mkPokemon machamp [Guts,NoGuard] WhiteHerb superpower stoneEdge firePunch bulletPunch

blue'sAlakazam =
  mkPokemon alakazam [Synchronize,InnerFocus] FocusSash psychic focusBlast shadowBall reflect

blue'sExeggutor =
  mkPokemon exeggutor [Chlorophyll] LifeOrb leafStorm woodHammer zenHeadbutt leechSeed

blue'sArcanine =
  mkPokemon arcanine [Intimidate,FlashFire] ExpertBelt flareBlitz closeCombat wildCharge extremeSpeed

blue'sGyarados =
  mkPokemon gyarados [Intimidate] KingsRock waterfall earthquake iceFang outrage

lance'sDragonite =
  mkPokemon dragonite [InnerFocus] FocusSash extremeSpeed icePunch firePunch dracoMeteor

lance'sSalamence =
  mkPokemon salamence [Intimidate] ExpertBelt dragonClaw earthquake stoneEdge crunch

lance'sKingdra =
  mkPokemon kingdra [SwiftSwim,Sniper] ScopeLens surf dragonPulse iceBeam flashCannon

lance'sHaxorus =
  mkPokemon haxorus [Rivalry,MoldBreaker] ChoiceScarf outrage superpower earthquake rockSlide

lance'sHydreigon =
  mkPokemon hydreigon [Levitate] WhiteHerb dracoMeteor darkPulse fireBlast earthPower

lance'sFlygon =
  mkPokemon flygon [Levitate] PowerHerb dracoMeteor earthPower solarBeam uTurn

steven'sMetagross =
  mkPokemon metagross [ClearBody] OccaBerry zenHeadbutt hammerArm earthquake bulletPunch

steven'sAggron =
  mkPokemon aggron [Sturdy,RockHead] AirBalloon headSmash avalanche earthquake metalBurst

steven'sExcadrill =
  mkPokemon excadrill [SandRush,SandForce] FocusSash earthquake rockSlide xScissor sandstorm

steven'sArcheops =
  mkPokemon archeops [Defeatist] SitrusBerry headSmash acrobatics earthquake quickAttack

steven'sCradily =
  mkPokemon cradily [SuctionCups] ExpertBelt stoneEdge seedBomb earthquake sandstorm

steven'sArmaldo =
  mkPokemon armaldo [BattleArmor] WhiteHerb xScissor rockBlast earthquake superpower

wallace'sMilotic =
  mkPokemon milotic [MarvelScale] RockyHelmet scald icyWind rest $
  sleepTalk scald icyWind rest

wallace'sSharpedo =
  mkPokemon sharpedo [RoughSkin] FocusSash hydroPump crunch zenHeadbutt aquaJet

wallace'sWalrein =
  mkPokemon walrein [ThickFat,IceBody] Leftovers surf yawn blizzard sheerCold

wallace'sLudicolo =
  mkPokemon ludicolo [SwiftSwim,RainDish] LifeOrb surf focusBlast gigaDrain rainDance

wallace'sSwampert =
  mkPokemon swampert [Torrent] RindoBerry muddyWater earthPower iceBeam focusBlast

wallace'sStarmie =
  mkPokemon starmie [Illuminate,NaturalCure] ExpertBelt surf psychic thunderbolt signalBeam

cynthia'sGarchomp =
  mkPokemon garchomp [SandVeil] FocusSash outrage earthquake stoneEdge swordsDance

cynthia'sSpiritomb =
  mkPokemon spiritomb [Pressure] RockyHelmet suckerPunch protect willOWisp painSplit

cynthia'sRoserade =
  mkPokemon roserade [NaturalCure,PoisonPoint] WhiteHerb leafStorm sludgeBomb shadowBall sleepPowder

cynthia'sTogekiss =
  mkPokemon togekiss [Hustle,SereneGrace] Leftovers airSlash auraSphere shadowBall grassKnot

cynthia'sLucario =
  mkPokemon lucario [Steadfast,InnerFocus] LifeOrb closeCombat darkPulse stoneEdge extremeSpeed

cynthia'sGlaceon =
  mkPokemon glaceon [SnowCloak] ChoiceScarf iceBeam shadowBall signalBeam waterPulse

alder'sVolcarona =
  mkPokemon volcarona [FlameBody] ChartiBerry heatWave bugBuzz psychic quiverDance

alder'sConkeldurr =
  mkPokemon conkeldurr [Guts,SheerForce] LifeOrb hammerArm payback stoneEdge machPunch

alder'sReuniclus =
  mkPokemon reuniclus [Overcoat,MagicGuard] Leftovers psychic reflect toxic lightScreen

alder'sKrookodile =
  mkPokemon krookodile [Intimidate,Moxie] ExpertBelt earthquake crunch stoneEdge outrage

alder'sChandelure =
  mkPokemon chandelure [FlashFire,FlameBody] ChoiceScarf flamethrower shadowBall psychic energyBall

alder'sBraviary =
  mkPokemon braviary [KeenEye,SheerForce] ChoiceBand braveBird superpower rockSlide uTurn

allPokemons :: [Pokemon Move]
allPokemons =
  [
    red'sPikachu,
    red'sLapras, 
    red'sSnorlax, 
    red'sVenusaur, 
    red'sCharizard, 
    blue'sAerodactyl, 
    blue'sMachamp, 
    blue'sAlakazam, 
    blue'sExeggutor, 
    blue'sArcanine, 
    blue'sGyarados,
    lance'sDragonite, 
    lance'sSalamence, 
    lance'sKingdra, 
    lance'sHaxorus,
    lance'sHydreigon, 
    lance'sFlygon,
    steven'sMetagross, 
    steven'sAggron,
    steven'sExcadrill, 
    steven'sArcheops, 
    steven'sCradily, 
    steven'sArmaldo,
    wallace'sMilotic,
    wallace'sSharpedo, 
    wallace'sWalrein,
    wallace'sLudicolo,
    wallace'sSwampert, 
    wallace'sStarmie, 
    cynthia'sGarchomp,
    cynthia'sSpiritomb,
    cynthia'sRoserade, 
    cynthia'sTogekiss, 
    cynthia'sLucario, 
    cynthia'sGlaceon, 
    alder'sVolcarona, 
    alder'sConkeldurr, 
    alder'sReuniclus, 
    alder'sKrookodile,
    alder'sChandelure, 
    alder'sBraviary 
  ]
