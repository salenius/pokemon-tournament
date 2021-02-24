module Domain.Entity.Trainer where

import Domain.Entity.Pokemon
import Domain.Entity.Pokemon.Species
import Domain.Entity.Move
import Domain.Attribute.Ability as Ab
import Domain.Attribute.HeldItem as I
import Domain.Attribute.PokemonFactors
import Domain.Attribute.TrainerFactors
import Control.Lens

data Trainer =
  Red
  | Blue
  | Lance
  | Steven
  | Wallace
  | Cynthia
  | Alder
  | Trainer CustomTrainer
  deriving (Eq)

data CustomTrainer = CustomTrainer
  {
    _customTrainerName :: String
  , _customTrainerParty :: Party (Pokemon Move)
  , _customTrainerClass :: TrainerClass
  , _customTrainerRegion :: Region
  } deriving (Eq,Show)

data Party p =
  PartyOf1 p
  | PartyOf2 p p
  | PartyOf3 p p p
  | PartyOf4 p p p p
  | PartyOf5 p p p p p
  | PartyOf6 p p p p p p
  deriving (Eq,Ord,Read)

instance Show p => Show (Party p) where
  show (PartyOf1 p) = show $ [p]
  show (PartyOf2 p p') = show $ [p, p']
  show (PartyOf3 a b c) = show $ [a,b,c]
  show (PartyOf4 a b c d) = show $ [a,b,c,d]
  show (PartyOf5 a b c d e) = show $ [a,b,c,d,e]
  show (PartyOf6 a b c d e f) = show $ [a,b,c,d,e,f]

instance Functor Party where
  fmap f (PartyOf1 p) = PartyOf1 . f $ p
  fmap f (PartyOf2 p p') = PartyOf2 (f p) (f p')
  fmap f (PartyOf3 a b c) = PartyOf3 (f a) (f b) $ f c
  fmap f (PartyOf4 a b c d) = PartyOf4 (f a) (f b) (f c) $ f d
  fmap f (PartyOf5 a b c d e) = PartyOf5 (f a) (f b) (f c) (f d) $ f e
  fmap f (PartyOf6 a b c d e g) = PartyOf6 (f a) (f b) (f c) (f d) (f e) $ f g

instance Foldable Party where
  foldr f acc (PartyOf1 p) = f p acc
  foldr f acc (PartyOf2 p p') = foldr f (f p' acc) (PartyOf1 p)
  foldr f acc (PartyOf3 b c a) = foldr f (f a acc) (PartyOf2 b c)
  foldr f acc (PartyOf4 b c d a) = foldr f (f a acc) (PartyOf3 b c d)
  foldr f acc (PartyOf5 b c d e a) = foldr f (f a acc) (PartyOf4 b c d e)
  foldr f acc (PartyOf6 b c d e g a) = foldr f (f a acc) (PartyOf5 b c d e g)


instance Show Trainer where
  show Red     = "Trainer: Red from Kanto (Champion)"
  show Blue    = "Trainer: Blue from Kanto (Champion)"
  show Lance   = "Trainer: Lance from Johto (Champion)"
  show Steven  = "Trainer: Steven from Hoenn (Champion)"
  show Wallace = "Trainer: Wallace from Hoenn (Champion)"
  show Cynthia = "Trainer: Cynthia from Sinnoh (Champion)"
  show Alder   = "Trainer: Alder from Unova (Champion)"
  show (Trainer (CustomTrainer n _ r c)) = "Trainer: " ++ n ++ " from " ++ show r ++ " (" ++ show c ++ ")"


mkPokemon :: PokemonSpecies' -> I.HeldItem -> Move -> Move -> Move -> Move -> Pokemon Move
mkPokemon species item mv1 mv2 mv3 mv4 =
  Pokemon species item (Quadruple mv1 mv2 mv3 mv4) defaultLevel defaultNature defaultEVs defaultIVs

