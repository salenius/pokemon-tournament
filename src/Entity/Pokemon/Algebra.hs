{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}


module Entity.Pokemon.Algebra where

import Attribute.Ability hiding (ability)
import Attribute.Gender
import Control.Lens
import Types.Pokemon
import Types.BuiltIn
import Control.Monad.State

class Monad m => PokemonBuild m where
  name :: String -> m ()
  hp :: Int -> m ()
  attack :: Int -> m()
  defence :: Int -> m ()
  sattack :: Int -> m ()
  sdefence :: Int -> m ()
  speed :: Int -> m ()
  typeOfIs :: PokemonType -> m ()
  typeof :: TypePut a => a -> m ()
  typeof = typeOfIs . putType
  ability :: (Show a) => a -> m ()
  abilities :: (Show a, Show b) => a -> b -> m ()
  abilities a b = do
    ability a
    ability b
  hiddenAbility :: (Show a) => a -> m ()
  weight :: Double -> m ()
  height :: Double -> m ()
  gender :: Gender -> Double -> m ()
  male :: Double -> m ()
  male = gender Male
  genderless :: m ()
  genderless = gender Genderless 1.0
  legendary :: m ()
  pseudoLegendary :: m ()
  megaEvolution :: m Ready -> a -> m ()
  end :: m Ready

type Pokemon = forall pkmn. (PokemonBuild pkmn) => pkmn Ready


class TypePut a where
  putType :: a -> PokemonType

instance TypePut TypeOf where
  putType = ListOf1

instance TypePut (TypeOf, TypeOf) where
  putType (a,b) = ListOf2 a b

data Form =
  Alolan
  | Galarian
  
data Ready = NotReady | Ready deriving (Eq,Show,Ord,Enum)

(%) :: Double -> (Double -> m ()) -> m ()
v % action = action $ v / 100 

instance PokemonBuild (State String) where
  name n = modify ((++) n)
  hp = modifdesc "HP" 
  attack = modifdesc "Attack"
  defence = modifdesc "Defence"
  sattack = modifdesc "Sp.Attack"
  sdefence = modifdesc "Sp.Defence"
  speed = modifdesc "Speed"
  typeOfIs n = modifdesc "Type" (case pokemonTypeToList n of
                                   [x]   -> show x
                                   [x,y] -> show x ++ " and " ++ show y
                                   _     -> "")
  ability = modifdesc "Ability"
  hiddenAbility = modifdesc "Hidden Ability"
  weight n = do
    modifdesc "Weight" n
    modify $ flip (++) " kg"
  height n = do
    modifdesc "Height" n
    modify $ flip (++) " m" 
  gender Genderless _ = modifdesc "Gender" "Genderless"
  gender Male       n = modifdesc "Gender" $ "Male " ++ (show $ 100 * n) ++ " %, Female " ++ (show $ 100 * (1-n)) ++ " %"
  gender Female     n = modifdesc "Gender" $ "Female " ++ (show $ 100 * n) ++ " %, Male " ++ (show $ 100 * (1-n)) ++ " %"
  legendary = modify $ flip (++) "Legendary Pokemon"
  pseudoLegendary = modify $ flip (++) "Pseudolegendary Pokemon"
  megaEvolution _ _ = return ()
  end = return Ready

desc :: Show a => String -> a -> String -> String
desc d n x = x ++ "\n" ++ "  " ++ " " ++ d ++ ": " ++ show n

modifdesc :: Show a => String -> a -> State String ()
modifdesc d n = modify (desc d n)

pokemonToString :: Pokemon -> String
pokemonToString pkmn = execState pkmn ""

describe :: Pokemon -> IO ()
describe pkmn = putStrLn $ execState pkmn ""
