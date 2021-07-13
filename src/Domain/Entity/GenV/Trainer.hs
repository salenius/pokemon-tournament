{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Entity.GenV.Trainer where

import Prelude hiding ((.))
import Domain.Entity.Common
import Domain.Entity.Trainer as T
import Domain.Entity.GenV.Item
import Domain.Entity.GenV.Pokemon
import Domain.Entity.GenV.Move hiding (name)
import Domain.Entity.Pokemon (move1,move2,move3,move4,PokemonMoveAlgebra, heldItem, level, ev, iv,Stat(..))
import qualified Domain.Entity.Move as Mv

import Domain.Entity.GenV.TypeOf as T
import Domain.Entity.GenV.Effect as Eff hiding (Stat(..))
import Domain.Entity.GenV.Damage as D
import Domain.Entity.GenV.Success as S
import Domain.Entity.GenV.Hit

type Pokemon' ch cn = Pokemon ch MoveSuccess MoveHit (DamageLogic cn) (Effect Battle)

type Item ch = ChampionsTournamentItem ch (Effect Battle)

data ChampionClass = ChampionClass {region :: String} deriving (Eq,Show,Ord)

type ChampionTrainer ch cn =
  Trainer (Pokemon' ch cn) ChampionClass (Item ch)

instance TrainerAlgebra (ChampionTrainer ch cn) (Pokemon ch MoveSuccess MoveHit (DamageLogic cn) (Effect Battle)) ChampionClass (Item ch) where
  name s = Branch (Name s)
  trainerClass c = Branch (Class c)
  pokemon1 p = Branch (Pokemon1 p)
  pokemon2 p = Branch (Pokemon2 p)
  pokemon3 p = Branch (Pokemon3 p)
  pokemon4 p = Branch (Pokemon4 p)
  pokemon5 p = Branch (Pokemon5 p)
  pokemon6 p = Branch (Pokemon6 p)
  addItem i = Branch (AddItem i)

red :: ChampionTrainer ch cn
red =
  name "Red" .
  trainerClass (ChampionClass "Kanto") .
  pokemon1 p1 .
  pokemon2 p2 .
  pokemon3 p3 .
  pokemon4 p4 .
  pokemon5 p5 .
  pokemon6 p6 $
  End
  where
    p1 = moves voltTackle ironTail fakeOut brickBreak .
      holds LightBall $ pikachu
    p2 = moves iceBeam hydroPump iceShard thunderbolt .
      holds SitrusBerry $ lapras
    p3 = moves bodySlam earthquake crunch seedBomb .
      holds QuickClaw $ snorlax
    p4 = moves leafStorm sludgeBomb earthquake sleepPowder .
      holds WhiteHerb $ venusaur
    p5 = moves fireBlast focusBlast airSlash dragonPulse .
      holds FocusSash $ charizard
    p6 = moves waterSpout hydroPump blizzard focusBlast .
      holds ChoiceScarf $ blastoise

blue :: ChampionTrainer ch cn
blue =
  name "Blue" .
  trainerClass (ChampionClass "Kanto") .
  pokemon1 p1 .
  pokemon2 p2 .
  pokemon3 p3 .
  pokemon4 p4 .
  pokemon5 p5 .
  pokemon6 p6 $
  End
  where
    p1 = moves stoneEdge earthquake iceFang fireBlast . holds ChoiceBand $ aerodactyl
    p2 = moves leafStorm woodHammer zenHeadbutt leechSeed . holds LifeOrb $ exeggutor
    p3 = moves waterfall earthquake iceFang outrage . holds KingsRock $ gyarados
    p4 = moves psychic focusBlast shadowBall reflect . holds FocusSash $ alakazam
    p5 = moves flareBlitz closeCombat wildCharge extremeSpeed . holds ExpertBelt $ arcanine
    p6 = moves superpower stoneEdge firePunch bulletPunch . holds WhiteHerb $ machamp

lance :: ChampionTrainer ch cn
lance =
  name "Lance" .
  trainerClass (ChampionClass "Johto") .
  pokemon1 p1 .
  pokemon2 p2 .
  pokemon3 p3 .
  pokemon4 p4 .
  pokemon5 p5 .
  pokemon6 p6 $
  End
  where
    p1 = moves extremeSpeed icePunch firePunch dracoMeteor . holds FocusSash $ dragonite
    p2 = moves dragonClaw earthquake stoneEdge crunch . holds ExpertBelt $ salamence
    p3 = moves surf dragonPulse iceBeam flashCannon . holds ScopeLens $ kingdra
    p4 = moves outrage superpower earthquake rockSlide . holds ChoiceScarf $ haxorus
    p5 = moves dracoMeteor darkPulse fireBlast earthPower . holds WhiteHerb $ hydreigon
    p6 = moves dracoMeteor earthPower solarBeam uTurn . holds PowerHerb $ flygon

steven :: ChampionTrainer ch cn
steven =
  name "Steven" .
  trainerClass (ChampionClass "Hoenn") .
  pokemon1 p1 .
  pokemon2 p2 .
  pokemon3 p3 .
  pokemon4 p4 .
  pokemon5 p5 .
  pokemon6 p6 $
  End
  where
    p1 = moves zenHeadbutt hammerArm earthquake bulletPunch . holds OccaBerry $ metagross
    p2 = moves headSmash avalanche earthquake metalBurst . holds AirBalloon $ aggron
    p3 = moves earthquake rockSlide xScissor sandstorm . holds FocusSash $ excadrill
    p4 = moves headSmash acrobatics earthquake quickAttack . holds SitrusBerry $ archeops
    p5 = moves stoneEdge seedBomb earthquake sandstorm . holds ExpertBelt $ cradily
    p6 = moves xScissor rockBlast earthquake superpower . holds WhiteHerb $ armaldo

wallace :: ChampionTrainer ch cn
wallace =
  name "Wallace" .
  trainerClass (ChampionClass "Hoenn") .
  pokemon1 p1 .
  pokemon2 p2 .
  pokemon3 p3 .
  pokemon4 p4 .
  pokemon5 p5 .
  pokemon6 p6 $
  End
  where
    p1 = moves scald icyWind rest sleepTalk . holds RockyHelmet $ milotic
    p2 = moves hydroPump crunch zenHeadbutt aquaJet . holds FocusSash $ sharpedo
    p3 = moves surf yawn blizzard sheerCold . holds Leftovers $ walrein
    p4 = moves surf focusBlast gigaDrain rainDance . holds LifeOrb $ ludicolo
    p5 = moves muddyWater earthPower iceBeam focusBlast . holds RindoBerry $ swampert
    p6 = moves surf psychic thunderbolt signalBeam . holds ExpertBelt $ starmie

cynthia :: ChampionTrainer ch cn
cynthia =
  name "Cynthia" .
  trainerClass (ChampionClass "Sinnoh") .
  pokemon1 p1 .
  pokemon2 p2 .
  pokemon3 p3 .
  pokemon4 p4 .
  pokemon5 p5 .
  pokemon6 p6 $
  End
  where
    p1 = moves outrage earthquake stoneEdge swordsDance . holds FocusSash $ garchomp
    p2 = moves suckerPunch protect willOWisp painSplit . holds RockyHelmet $ spiritomb
    p3 = moves leafStorm sludgeBomb shadowBall sleepPowder . holds WhiteHerb $ roserade
    p4 = moves airSlash auraSphere shadowBall grassKnot . holds Leftovers $ togekiss
    p5 = moves closeCombat darkPulse stoneEdge extremeSpeed . holds LifeOrb $ lucario
    p6 = moves iceBeam shadowBall signalBeam waterPulse . holds ChoiceScarf $ glaceon

alder :: ChampionTrainer ch cn
alder =
  name "Alder" .
  trainerClass (ChampionClass "Unova") .
  pokemon1 p1 .
  pokemon2 p2 .
  pokemon3 p3 .
  pokemon4 p4 .
  pokemon5 p5 .
  pokemon6 p6 $
  End
  where
    p1 = moves heatWave bugBuzz psychic quiverDance . holds ChartiBerry $ volcarona
    p2 = moves hammerArm payback stoneEdge machPunch . holds LifeOrb $ conkeldurr
    p3 = moves psychic reflect toxic lightScreen . holds Leftovers $ reuniclus
    p4 = moves earthquake crunch stoneEdge outrage . holds ExpertBelt $ krookodile
    p5 = moves flamethrower shadowBall psychic energyBall . holds ChoiceScarf $ chandelure
    p6 = moves braveBird superpower rockSlide uTurn . holds ChoiceBand $ braviary

----

moves :: Move ch cn -> Move ch cn -> Move ch cn -> Move ch cn -> Pokemon' ch cn -> Pokemon' ch cn
moves m1 m2 m3 m4 = move1 m1 . move2 m2 . move3 m3 . move4 m4

holds :: ChampionsTournamentItem ch (Effect Battle) -> Pokemon' ch cn -> Pokemon' ch cn
holds i = level 50 . heldItem i

evSpread :: Stat -> Stat -> Stat -> Pokemon' ch cn -> Pokemon' ch cn
evSpread s1 s2 s3 = ev s1 252 . ev s2 252 . ev s3 4

physicalSweeper = evSpread Attack Speed Defence
specialSweeper = evSpread SAttack Speed Defence
