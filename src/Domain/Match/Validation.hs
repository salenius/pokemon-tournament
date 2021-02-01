module Domain.Match.Validation where

import Domain.Attribute.Ailment
import Domain.Attribute.Ability
import Domain.Attribute.HeldItem
import Domain.Attribute.Counterparty
import Domain.Attribute.ModifStat
import Domain.Attribute.MoveExecution
import Domain.Attribute.Weather
import Domain.Attribute.TypeOf

data Logic a =
  Statement a
  | Not (Logic a) 
  | And (Logic a) (Logic a)
  | Or (Logic a) (Logic a)
  | AlwaysTrue
  deriving (Eq,Show)

data BattleValidation =
  AbilityIs Counterparty (Ability -> Bool) 
  | HeldItemIs Counterparty (Maybe HeldItem -> Bool) 
  | WeatherIs (Weather -> Bool) 
  | TypeIs Counterparty ([TypeOf] -> Bool) 
  | MoveExecutionIs (MoveExecution -> Bool)
  | AilmentIs Counterparty (Ailment -> Bool)
  | UserStrikesSecond --Paybackia varten
  | TargetUsedItem --Paybackia varten, jos nykyinen target käytti ensin esineen

type Validation = Logic BattleValidation

abilityIs cp = Statement . AbilityIs cp
heldItemIs cp = Statement . HeldItemIs cp
weatherIs = Statement . WeatherIs
typeIs cp = Statement . TypeIs cp
moveExecutionIs = Statement . MoveExecutionIs
ailmentIs cp = Statement . AilmentIs cp
userStrikesSecond = Statement UserStrikesSecond
targetUsedItem = Statement TargetUsedItem
  
instance Eq BattleValidation where
  AbilityIs cp fn == AbilityIs cp' fn' =
    cp == cp' && filter fn allAbilities == filter fn' allAbilities
  HeldItemIs cp fn == HeldItemIs cp' fn' =
    cp == cp' && filter fn allItems == filter fn' allItems
    where
      allItems = map Just allHeldItems ++ [Nothing]
  WeatherIs fn == WeatherIs fn' = filter fn allWeathers == filter fn' allWeathers
  TypeIs cp fn == TypeIs cp' fn' =
    cp == cp' && filter fn allTypeCombinations == filter fn' allTypeCombinations
  (==) _ _ = False

instance Show BattleValidation where
  show (AbilityIs cp fn) = "AbilityIs " ++ show cp ++ " fn :: Ability -> Bool"
  show (HeldItemIs cp fn) = "HeldItemIs " ++ show cp ++ " fn :: Maybe HeldItem -> Bool"
  show (WeatherIs fn) = "WeatherIs fn :: Weather -> Bool"
  show (TypeIs cp fn) = "TypeIs " ++ show cp ++ " fn :: [TypeOf] -> Bool"
  show (AilmentIs cp fn) = "AilmentIs " ++ show cp ++ " fn :: Ailment -> Bool"
  show (MoveExecutionIs fn) = "MoveExecutionIs fn :: MoveExecution -> Bool"

-- Yleiseen käyttöön funktiot
  
moldBreaker :: Validation
moldBreaker = User `abilityIs` (\ab -> ab `elem` [MoldBreaker])

notMoldBreaker = Not moldBreaker
