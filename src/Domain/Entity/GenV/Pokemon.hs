{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Domain.Entity.GenV.Pokemon where

import Prelude hiding ((.))
import Domain.Entity.Common
import qualified Domain.Entity.Pokemon as P
import Domain.Entity.Pokemon hiding (Pokemon)
import Domain.Entity.GenV.Item
import Domain.Entity.GenV.Ability
import Domain.Entity.GenV.TypeOf
import qualified Domain.Entity.Move as M

type Ability = ChampionsTournamentAbility

type Pokemon choice success prob damage eff =
  P.Pokemon TypeOf Ability (ChampionsTournamentItem choice eff) (M.Move TypeOf choice success prob damage eff)

instance PokemonAbilityAlgebra (Pokemon c s p d e) ChampionsTournamentAbility where
  ability ab = wrap (AddAbility (SlotAbility ab))

instance PokemonItemAlgebra (Pokemon c s p d e) (ChampionsTournamentItem c e) where
  heldItem i = wrap (HeldItem i)

instance PokemonTypeAlgebra (Pokemon c s p d e) TypeOf where
  type1 t = wrap (Type1 t)
  type2 t = wrap (Type2 t)

instance PokemonMoveAlgebra (Pokemon c s p d e) (M.Move TypeOf c s p d e) where
  move1 m = wrap (AddMove Move1 m)
  move2 m = wrap (AddMove Move2 m)
  move3 m = wrap (AddMove Move3 m)
  move4 m = wrap (AddMove Move4 m)

--------------------------------------------

pikachu :: Pokemon choice success prob damage eff
pikachu =                              
  name "Pikachu" .                     
  hp 35 .                              
  attack 55 .                          
  defence 40 .                         
  sattack 50 .                         
  sdefence 50 .                        
  speed 90 .                           
  type1 Electric .                    
  50 % male .                          
  ability Static .                     
  weight 6.0 .                         
  height 0.4 $
  end

lapras :: Pokemon choice success prob damage eff
lapras =                               --
  name "Lapras" .                      --
  hp 130 .                             --
  attack 85 .                          --
  defence 80 .                         --
  sattack 85 .                         --
  sdefence 95 .                        --
  speed 60 .                           --
  typeof (Water, Ice) .                --
  50 % male .                          --
  abilities WaterAbsorb ShellArmor .   --
  weight 220.0 .                       --
  height 2.5 $                         --
  end                                  --

snorlax :: Pokemon choice success prob damage eff                                      --
snorlax =                              --
  name "Snorlax" .                     --
  hp 160 .                             --
  attack 110 .                         --
  defence 65 .                         --
  sattack 65 .                         --
  sdefence 110 .                       --
  speed 30 .                           --
  type1 Normal .                      --
  87.5 % male .                        --
  abilities Immunity ThickFat .        --
  weight 460.0 .                       --
  height 2.1 $                         --
  end                                  --
--                                        --
venusaur :: Pokemon choice success prob damage eff
venusaur =                             --
  name "Venusaur" .                    --
  hp 80 .                              --
  attack 82 .                          --
  defence 83 .                         --
  sattack 100 .                        --
  sdefence 100 .                       --
  speed 80 .                           --
  typeof (Grass, Poison) .             --
  87.5 % male .                        --
  ability Overgrow .                   --
  weight 100.0 .                       --
  height 2.0 $                         --
  end                                  --
--                                        --
charizard :: Pokemon choice success prob damage eff
charizard =                            --
  name "Charizard" .                   --
  hp 78 .                              --
  attack 84 .                          --
  defence 78 .                         --
  sattack 109 .                        --
  sdefence 85 .                        --
  speed 100 .                          --
  typeof (Fire, Flying) .              --
  87.5 % male .                        --
  ability Blaze .                      --
  weight 90.5 .                        --
  height 1.7 $                         --
  end                                  --
--                                        --
blastoise :: Pokemon choice  success prob damage  eff
blastoise =                            --
  name "Blastoise" .                   --
  hp 79 .                              --
  attack 83 .                          --
  defence 100 .                        --
  sattack 85 .                         --
  sdefence 105 .                       --
  speed 78 .                           --
  type1 Water .                       --
  87.5 % male .                        --
  ability Torrent .                    --
  weight 85.5 .                        --
  height 1.6 $                         --
  end                                  --
--                                        --
aerodactyl :: Pokemon choice  success prob damage  eff
aerodactyl =                           --
  name "Aerodactyl" .                  --
  hp 80 .                              --
  attack 105 .                         --
  defence 65 .                         --
  sattack 60 .                         --
  sdefence 75 .                        --
  speed 130 .                          --
  typeof (Rock, Flying) .              --
  87.5 % male .                        --
  abilities RockHead Pressure .        --
  weight 59.0 .                        --
  height 1.6 $                         --
  end                                  --
--                                        --
machamp :: Pokemon choice  success prob damage  eff
machamp =                              --
  name "Machamp" .                     --
  hp 90 .                              --
  attack 130 .                         --
  defence 80 .                         --
  sattack 65 .                         --
  sdefence 85 .                        --
  speed 55 .                           --
  type1 Fighting .                    --
  75 % male .                          --
  abilities Guts NoGuard .             --
  weight 130.0 .                       --
  height 1.6 $                         --
  end                                  --
--                                        --
alakazam :: Pokemon choice  success prob damage  eff
alakazam =                             --
  name "Alakazam" .                    --
  hp 55 .                              --
  attack 50 .                          --
  defence 45 .                         --
  sattack 135 .                        --
  sdefence 95 .                        --
  speed 120 .                          --
  type1 Psychic .                     --
  75 % male .                          --
  abilities Synchronize InnerFocus .   --
  weight 48.0 .                        --
  height 1.5 $                         --
  end                                  --
--                                        --
exeggutor :: Pokemon choice  success prob damage  eff
exeggutor =                            --
  name "Exeggutor" .                   --
  hp 95 .                              --
  attack 95 .                          --
  defence 85 .                         --
  sattack 125 .                        --
  sdefence 75 .                        --
  speed 55 .                           --
  typeof (Grass, Psychic) .            --
  50 % male .                          --
  ability Chlorophyll .                --
  weight 120.0 .                       --
  height 2.0 $                         --
  end                                  --
--                                        --
arcanine :: Pokemon choice  success prob damage  eff
arcanine =                             --
  name "Arcanine" .                    --
  hp 90 .                              --
  attack 110 .                         --
  defence 80 .                         --
  sattack 100 .                        --
  sdefence 80 .                        --
  speed 95 .                           --
  type1 Fire .                        --
  75 % male .                          --
  abilities Intimidate FlashFire .     --
  weight 155.0 .                       --
  height 1.9 $                         --
  end                                  --
--                                        --
gyarados :: Pokemon choice  success prob damage  eff
gyarados =                             --
  name "Gyarados" .                    --
  hp 95 .                              --
  attack 125 .                         --
  defence 79 .                         --
  sattack 60 .                         --
  sdefence 100 .                       --
  speed 81 .                           --
  typeof (Water, Flying) .             --
  50 % male .                          --
  ability Intimidate .                 --
  weight 235.0 .                       --
  height 6.5 $                         --
  end                                  --
--                                        --
dragonite :: Pokemon choice  success prob damage  eff
dragonite =                            --
  name "Dragonite" .                   --
  hp  91   .                           --
  attack  134   .                      --
  defence  95 .                        --
  sattack 100 .                        --
  sdefence 100   .                     --
  speed  80 .                          --
  typeof (Dragon, Flying) .            --
  50 % male .                          --
  height  2.2 .                        --
  weight  210.0 .                      --
  ability InnerFocus $                 --
  end                                  --
--                                        --
salamence :: Pokemon choice  success prob damage  eff
salamence =                            --
  name "Salamence" .                   --
  hp  95  .                            --
  attack  135   .                      --
  defence  80 .                        --
  sattack 110 .                        --
  sdefence 80   .                      --
  speed  100 .                         --
  typeof (Dragon, Flying) .           --
  50 % male .                          --
  height  1.5 .                        --
  weight  102.6 .                      --
  ability Intimidate $                 --
  end                                  --
--                                        --
kingdra :: Pokemon choice  success prob damage  eff
kingdra =                              --
  name "Kingdra" .                     --
  hp  75   .                           --
  attack  95   .                       --
  defence  95 .                        --
  sattack 95 .                         --
  sdefence 95   .                      --
  speed  85 .                          --
  typeof (Water, Dragon) .            --
  50 % male .                          --
  height  1.8 .                        --
  weight  152.0 .                      --
  abilities SwiftSwim Sniper $         --
  end                                  --
--                                        --
haxorus :: Pokemon choice  success prob damage  eff
haxorus =                              --
  name "Haxorus" .                     --
  hp  75   .                           --
  attack  95   .                       --
  defence  95 .                        --
  sattack 95 .                         --
  sdefence 95   .                      --
  speed  85 .                          --
  type1  Dragon .                     --
  50 % male .                          --
  height  1.8 .                        --
  weight  105.5 .                      --
  abilities  Rivalry MoldBreaker $     --
  end                                  --
--                                        --
hydreigon :: Pokemon choice  success prob damage  eff
hydreigon =                            --
  name "Hydreigon" .                   --
  hp  92   .                           --
  attack  105   .                      --
  defence  90 .                        --
  sattack 125 .                        --
  sdefence 90   .                      --
  speed  98 .                          --
  typeof (Dark, Dragon) .             --
  50 % male .                          --
  height 1.8 .                         --
  weight 160.0 .                       --
  ability Levitate $                   --
  end                                  --
--                                        --
flygon :: Pokemon choice  success prob damage  eff
flygon =                               --
  name "Flygon" .                      --
  hp  80 .                             --
  attack  100 .                        --
  defence  80 .                        --
  sattack 80 .                         --
  sdefence 80  .                       --
  speed  100 .                         --
  typeof (Ground, Dragon) .           --
  50 % male .                          --
  height  2.0 .                        --
  weight  82.0 .                       --
  ability  Levitate $                  --
  end                                  --
--                                        --
metagross :: Pokemon choice  success prob damage  eff
metagross =                            --
  name "Metagross" .                   --
  hp  80  .                            --
  attack  135  .                       --
  defence  130 .                       --
  sattack 95 .                         --
  sdefence 90  .                       --
  speed  70 .                          --
  typeof (Steel, Psychic) .             --
  genderless .                         --
  height  1.6 .                        --
  weight  550.0 .                      --
  ability ClearBody $                  --
  end                                  --
--                                        --
aggron :: Pokemon choice  success prob damage  eff
aggron =                               --
  name "Aggron" .                      --
  hp  70  .                            --
  attack  110  .                       --
  defence  180 .                       --
  sattack 60 .                         --
  sdefence 60  .                       --
  speed  50 .                          --
  typeof (Steel, Rock) .              --
  50 % male .                          --
  height  2.1 .                        --
  weight  360.0 .                      --
  abilities  Sturdy RockHead $         --
  end                                  --
--                                        --
excadrill :: Pokemon choice  success prob damage  eff
excadrill =                            --
  name "Excadrill" .                   --
  hp  110  .                           --
  attack  135  .                       --
  defence  60 .                        --
  sattack 50 .                         --
  sdefence 65  .                       --
  speed  88  .                         --
  typeof (Ground, Steel) .            --
  50 % male .                          --
  height  0.7 .                        --
  weight  40.4 .                       --
  abilities  SandRush SandForce $      --
  end                                  --
--                                        --
archeops :: Pokemon choice  success prob damage  eff
archeops =                             --
  name "Archeops" .                    --
  hp  75  .                            --
  attack  140  .                       --
  defence  65 .                        --
  sattack 112 .                        --
  sdefence 65  .                       --
  speed  110 .                         --
  typeof (Rock, Flying) .             --
  87.5 % male .                        --
  height  1.4 .                        --
  weight  32.0 .                       --
  ability  Defeatist $                 --
  end                                  --
--                                        --
cradily :: Pokemon choice  success prob damage  eff
cradily =                              --
  name "Cradily" .                     --
  hp  86  .                            --
  attack  81  .                        --
  defence  97 .                        --
  sattack 81 .                         --
  sdefence 107  .                      --
  speed  43 .                          --
  typeof (Rock, Grass) .              --
  87.5 % male .                        --
  height  1.5 .                        --
  weight  60.4 .                       --
  ability  SuctionCups $               --
  end                                  --
--                                        --
armaldo :: Pokemon choice  success prob damage  eff
armaldo =                              --
  name "Armaldo" .                     --
  hp  75 .                             --
  attack 125  .                        --
  defence  100 .                       --
  sattack 70 .                         --
  sdefence 80  .                       --
  speed  45 .                          --
  typeof (Rock, Bug) .                --
  87.5 % male .                        --
  height  1.5 .                        --
  weight  68.2 .                       --
  ability  BattleArmor $               --
  end                                  --
--                                        --
milotic :: Pokemon choice  success prob damage  eff
milotic =                              --
  name "Milotic" .                     --
  hp  95 .                             --
  attack 60  .                         --
  defence  79 .                        --
  sattack 100 .                        --
  sdefence 125  .                      --
  speed  81 .                          --
  type1  Water .                      --
  50 % male .                          --
  height  6.2 .                        --
  weight  162.0 .                      --
  abilities  MarvelScale Competitive $ --
  end                                  --
--                                        --
sharpedo :: Pokemon choice  success prob damage  eff
sharpedo =                             --
  name "Sharpedo" .                    --
  hp  70 .                             --
  attack 120  .                        --
  defence  40 .                        --
  sattack 95 .                         --
  sdefence 40  .                       --
  speed  95 .                          --
  typeof (Water, Dark) .              --
  50 % male .                          --
  height  1.8 .                        --
  weight  88.8 .                       --
  ability  RoughSkin $                 --
  end                                  --
--                                        --
walrein :: Pokemon choice  success prob damage  eff
walrein =                              --
  name "Walrein" .                     --
  hp  110 .                            --
  attack 80  .                         --
  defence  90 .                        --
  sattack 95 .                         --
  sdefence 90  .                       --
  speed  65 .                          --
  typeof (Ice, Water) .               --
  50 % male .                          --
  height  1.4 .                        --
  weight  150.6 .                      --
  abilities  ThickFat IceBody $        --
  end                                  --
--                                        --
ludicolo :: Pokemon choice  success prob damage  eff
ludicolo =                             --
  name "Ludicolo" .                    --
  hp  80 .                             --
  attack 70  .                         --
  defence  70 .                        --
  sattack 90 .                         --
  sdefence 100  .                      --
  speed  70 .                          --
  typeof (Water, Grass) .             --
  50 % male .                          --
  height  1.5 .                        --
  weight  55.0 .                       --
  abilities  SwiftSwim RainDish $      --
  end                                  --
--                                        --
swampert :: Pokemon choice  success prob damage  eff
swampert =                             --
  name "Swampert" .                    --
  hp  100 .                            --
  attack 110  .                        --
  defence  90 .                        --
  sattack 85 .                         --
  sdefence 90  .                       --
  speed  60 .                          --
  typeof (Water, Ground) .            --
  87.5 % male .                        --
  height  1.5 .                        --
  weight  81.9 .                       --
  ability  Torrent $                   --
  end                                  --
--                                        --
starmie :: Pokemon choice  success prob damage  eff
starmie =                              --
  name "Starmie" .                     --
  hp  60 .                             --
  attack 75  .                         --
  defence  85 .                        --
  sattack 100 .                        --
  sdefence 85  .                       --
  speed 115 .                           --
  genderless .                         --
  typeof (Water, Psychic) .           --
  height  1.1 .                        --
  weight  80.0 .                       --
  abilities  Illuminate NaturalCure $  --
  end                                  --
--                                        --
garchomp :: Pokemon choice  success prob damage  eff
garchomp =                             --
  name "Garchomp" .                    --
  hp  108 .                            --
  attack 130  .                        --
  defence  95 .                        --
  sattack 80 .                         --
  sdefence 85  .                       --
  speed  102 .                         --
  typeof (Dragon, Ground) .           --
  50 % male .                          --
  height  1.9 .                        --
  weight  95.0 .                       --
  ability  SandVeil $                  --
  end                                  --
--                                        --
spiritomb :: Pokemon choice  success prob damage  eff
spiritomb =                            --
  name "Spiritomb" .                   --
  hp  50 .                             --
  attack 92  .                         --
  defence  108 .                       --
  sattack 92 .                         --
  sdefence 108  .                      --
  speed  35 .                          --
  typeof (Ghost, Dark) .              --
  50 % male .                          --
  height  1.0 .                        --
  weight  108.0 .                      --
  ability  Pressure $                  --
  end                                  --
--                                        --
roserade :: Pokemon choice  success prob damage  eff
roserade =                             --
  name "Roserade" .                    --
  hp  60 .                             --
  attack 70  .                         --
  defence  65 .                        --
  sattack 125 .                        --
  sdefence 105  .                      --
  speed  90 .                          --
  typeof (Grass, Poison) .            --
  50 % male .                          --
  height  0.9 .                        --
  weight  14.5 .                       --
  abilities  NaturalCure PoisonPoint $ --
  end                                  --
--                                        --
togekiss :: Pokemon choice  success prob damage  eff
togekiss =                             --
  name "Togekiss" .                    --
  hp  85 .                             --
  attack 50  .                         --
  defence  95 .                        --
  sattack 120 .                        --
  sdefence 115  .                      --
  speed  80 .                          --
  typeof (Normal, Flying) .            --
  87.5 % male .                        --
  height  1.5 .                        --
  weight  38.0 .                       --
  abilities  Hustle SereneGrace $      --
  end                                  --
--                                        --
lucario :: Pokemon choice  success prob damage  eff
lucario =                              --
  name "Lucario" .                     --
  hp  70 .                             --
  attack 110  .                        --
  defence  70 .                        --
  sattack 115 .                        --
  sdefence 70  .                       --
  speed  90 .                          --
  typeof (Fighting, Steel) .          --
  87.5 % male .                        --
  height  1.2 .                        --
  weight  54.0 .                       --
  abilities  Steadfast InnerFocus $    --
  end                                  --
--                                        --
glaceon :: Pokemon choice  success prob damage  eff
glaceon =                              --
  name "Glaceon" .                     --
  hp  65 .                             --
  attack 60  .                         --
  defence  110 .                       --
  sattack 130 .                        --
  sdefence 95  .                       --
  speed  65 .                          --
  type1  Ice .                        --
  87.5 % male .                        --
  height  0.8 .                        --
  weight  25.9 .                       --
  ability  SnowCloak $                 --
  end                                  --
--                                        --
volcarona :: Pokemon choice  success prob damage  eff
volcarona =                            --
  name "Volcarona" .                   --
  hp  85 .                             --
  attack 60  .                         --
  defence  65 .                        --
  sattack 135 .                        --
  sdefence 105  .                      --
  speed  100 .                         --
  typeof (Bug, Fire) .                --
  50 % male .                          --
  height  1.6 .                        --
  weight  46.0 .                       --
  ability  FlameBody $                 --
  end                                  --
--                                        --
conkeldurr :: Pokemon choice  success prob damage  eff
conkeldurr =                           --
  name "Conkeldurr" .                  --
  hp  105 .                            --
  attack 140  .                        --
  defence  95 .                        --
  sattack 55 .                         --
  sdefence 65  .                       --
  speed  45 .                          --
  type1  Fighting .                   --
  75 % male .                          --
  height  1.4 .                        --
  weight  87.0 .                       --
  abilities  Guts SheerForce $         --
  end                                  --
--                                        --
reuniclus :: Pokemon choice  success prob damage  eff
reuniclus =                            --
  name "Reuniclus" .                   --
  hp  110 .                            --
  attack 65  .                         --
  defence  75 .                        --
  sattack 125 .                        --
  sdefence 85  .                       --
  speed  30 .                          --
  type1  Psychic .                    --
  50 % male .                          --
  height  1.0 .                        --
  weight  20.1 .                       --
  abilities  Overcoat MagicGuard $     --
  end                                  --
--                                        --
krookodile :: Pokemon choice  success prob damage  eff
krookodile =                           --
  name "Krookodile" .                  --
  hp  95 .                             --
  attack 117  .                        --
  defence  80 .                        --
  sattack 65 .                         --
  sdefence 70  .                       --
  speed  92 .                          --
  typeof (Ground, Dark) .             --
  50 % male .                          --
  height  1.5 .                        --
  weight  96.3 .                       --
  abilities  Intimidate Moxie $        --
  end                                  --
--                                        --
chandelure :: Pokemon choice  success prob damage  eff
chandelure =                           --
  name "Chandelure" .                  --
  hp  60 .                             --
  attack 55  .                         --
  defence  90 .                        --
  sattack 145 .                        --
  sdefence 90  .                       --
  speed  80 .                          --
  typeof (Ghost, Fire) .              --
  50 % male .                          --
  height  1.0 .                        --
  weight  34.3 .                       --
  abilities  FlashFire FlameBody $     --
  end                                  --
--                                        --
braviary :: Pokemon choice  success prob damage  eff
braviary =                             --
  name "Braviary" .                    --
  hp  100 .                            --
  attack 123  .                        --
  defence  75 .                        --
  sattack 57 .                         --
  sdefence 75  .                       --
  speed  80 .                          --
  typeof (Normal, Flying) .           --
  100 % male .                         --
  height  1.5 .                        --
  weight  41.0 .                       --
  abilities  KeenEye SheerForce $      --
  end                                  --
--------------------------------------------
 
