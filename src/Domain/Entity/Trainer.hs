{-# LANGUAGE FunctionalDependencies #-}

module Domain.Entity.Trainer where

import Domain.Entity.Common hiding ((.))

data TrainerAttribute pkmn clss item =
  Name String 
  | Class clss 
  | Pokemon1 pkmn 
  | Pokemon2 pkmn 
  | Pokemon3 pkmn 
  | Pokemon4 pkmn 
  | Pokemon5 pkmn 
  | Pokemon6 pkmn 
  | AddItem item 
  deriving (Eq,Show,Ord)

type Trainer pkmn clss item = AST (TrainerAttribute pkmn clss item)

class TrainerAlgebra trnr pkmn clss item | trnr -> pkmn, trnr -> clss, trnr -> item  where 
  name :: String -> trnr -> trnr
  trainerClass :: clss -> trnr -> trnr
  pokemon1 :: pkmn -> trnr -> trnr
  pokemon2 :: pkmn -> trnr -> trnr
  pokemon3 :: pkmn -> trnr -> trnr
  pokemon4 :: pkmn -> trnr -> trnr
  pokemon5 :: pkmn -> trnr -> trnr
  pokemon6 :: pkmn -> trnr -> trnr
  addItem :: item -> trnr -> trnr
