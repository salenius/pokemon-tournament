{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}


module Domain.Entity.Structured.Pokemon where

import Control.Lens
import Domain.Entity.Common hiding ((.))
import qualified Domain.Entity.Pokemon as P
import qualified Domain.Entity.Structured.Move as Mv

data Pokemon restpokemon typeof ability item move = Pokemon
  {
    _pokemonName :: String
  , _pokemonLevel :: Int
  , _pokemonType1 :: typeof
  , _pokemonType2 :: Maybe typeof
  , _pokemonBasestats :: Stats
  , _pokemonEVStats :: Stats
  , _pokemonIVStats :: Stats
  , _pokemonMove1 :: move
  , _pokemonMove2 :: Maybe move
  , _pokemonMove3 :: Maybe move
  , _pokemonMove4 :: Maybe move
  , _pokemonHeldItem :: Maybe item
  , _pokemonAbility1 :: ability
  , _pokemonAbility2 :: Maybe ability
  , _pokemonHiddenAbility :: Maybe ability
  , _pokemonWeight :: Double
  , _pokemonHeight :: Double
  , _pokemonNature :: Nature
  , _pokemonGender :: P.Gender
  , _pokemonOther :: restpokemon
  } deriving (Eq,Show,Ord)

data Stats = Stats
  {
    _hpStat :: Int
  , _attackStat :: Int
  , _defenceStat :: Int
  , _sAttackStat :: Int
  , _sDefenceStat :: Int
  , _speedStat :: Int
  } deriving (Eq,Show,Ord)

newtype Nature = Nature String deriving (Eq,Show,Ord)

makeLenses ''Stats
makeLenses ''Pokemon

---

filterElem :: (a -> Bool) -> AST a -> [a]
filterElem f mv = view astAsList (over astAsList (filter f) mv)

unambiguous :: (a -> Bool) -> (a -> b) -> s -> AST a -> Either s b
unambiguous g f s as = case length . filterElem g $ as of
                     ((>) 1 -> True) -> Left s
                     0 -> Left s
                     _ -> Right (f . head . filterElem g $ as)

combineAll :: (a -> Bool) -> (a -> b) -> AST a -> AST b
combineAll g f = view listAsAst . map f . filterElem g 

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left x) = Nothing
eitherToMaybe (Right x) = Just x

compress :: Maybe (Either a b) -> Maybe b
compress (Just (Left x)) = Nothing
compress (Just (Right x)) = Just x
compress Nothing = Nothing

compress' :: Either String (Either String b) -> Either String b
compress' (Right (Right x)) = Right x
compress' (Left _) = Left "--"
compress' (Right (Left x)) = Left x


--------

parseName = unambiguous (\case {P.Name n -> True; _ -> False}) (\(P.Name n) -> n) "Pokemon name can only appear once."

parseLevel = unambiguous f  (\(P.Level l) -> l) "Pokemon level can only appear once and must be within 1 and 100."
  where
    f = \case
      P.Level l -> l `elem` [1..100]
      _         -> False

parseType1 = unambiguous  (\case {P.Type1 n -> True; _ -> False}) (\(P.Type1 n) -> n) "Pokemon type1 can only appear once."

parseType2 = eitherToMaybe . unambiguous  (\case {P.Type2 n -> True; _ -> False}) (\(P.Type2 n) -> n) "Pokemon type2 can only appear once."

parseMove1 = compress' . parsingMove P.Move1 1

-- parsingMove :: P.MoveOp -> Int -> AST (P.PokemonAttribute typeof ability item move) -> Either String move
parsingMove mv nr = unambiguous f (\(P.AddMove _ n) -> Mv.parseMove n) ("Pokemon can fit only one move to slot #" ++ show nr ++ ".")
  where
    f (P.AddMove mv' n)
      | mv == mv' = True
      | otherwise = False
    f _ = False

parseMove2 = compress . eitherToMaybe . parsingMove P.Move2 2

parseMove3 = compress . eitherToMaybe . parsingMove P.Move3 3

parseMove4 = compress . eitherToMaybe . parsingMove P.Move4 4

data StatType = Base | IV' | EV' deriving (Eq,Show,Ord)

parsingStat
  :: StatType
  -> P.Stat
  -> AST (P.PokemonAttribute typeof ability item move)
  -> Either String Int
parsingStat statt stat = unambiguous (f statt) g "Pokemon can only have one stat"
  where
    f stt (P.Stat s i)
      | s == stat && i > 0 && stt == Base = True
      | otherwise = False
    f stt (P.IV s i)
      | s == stat && i > 0 && i < 252 && stt == IV' = True
      | otherwise = False
    f stt (P.EV s i)
      | s == stat && i > 0 && i < 32 && stt == EV' = True
      | otherwise = False
    f _ _ = False
    g (P.Stat _ i) = i

parseAllStats f ast =
  Stats <$>
  (f P.HP ast) <*>
  (f P.Attack ast) <*>
  (f P.Defence ast) <*>
  (f P.SAttack ast) <*>
  (f P.SDefence ast) <*>
  (f P.Speed ast)

parseWeight = unambiguous (\case {(P.Physiology (P.Weight (P.Kilograms n))) -> True; _ -> False}) (\(P.Physiology (P.Weight (P.Kilograms n))) -> n) "Pokemon weight can only appear once"

parseHeight = unambiguous (\case {(P.Physiology (P.Height (P.Meters n))) -> True; _ -> False}) (\(P.Physiology (P.Height (P.Meters n))) -> n) "Pokemon weight can only appear once"

parseHeldItem = eitherToMaybe . unambiguous (\case {(P.HeldItem i) -> True; _ -> False}) (\(P.HeldItem i) -> i) "Pokemon can only have one held item."

parseAbility1 = f . combineAll (\case {(P.AddAbility (P.SlotAbility ab)) -> True; _ -> False}) (\(P.AddAbility (P.SlotAbility ab)) -> ab)
  where
    f End = Left "Pokemon must have at least one ability."
    f (Branch ab y) = Right ab

parseAbility2 =
  eitherToMaybe .
  f .
  combineAll (\case {(P.AddAbility (P.SlotAbility ab)) -> True; _ -> False}) (\(P.AddAbility (P.SlotAbility ab)) -> ab)
  where
    f End = Left "Pokemon must have at least one ability."
    f (Branch ab End) = Right ab
    f (Branch ab y) = f y

parseHiddenAbility =
  eitherToMaybe .
  unambiguous (\case {(P.AddAbility (P.HiddenAbility ab)) -> True; _ -> False}) (\(P.AddAbility (P.HiddenAbility ab)) -> ab) "Pokemon can have at most one hidden ability"

parseGender =
  unambiguous (\case {(P.Gender g) -> True; _ -> False}) f "Pokemon can have only one gender policy."
  where
    f (P.Gender (P.ProbabilityOfFemale d)) = P.ProbabilityOfMale d
    f (P.Gender x) = x

parsePokemon ast =
  Pokemon <$>
  parseName ast <*>
  parseLevel ast <*>
  parseType1 ast <*>
  return (parseType2 ast) <*>
  parseAllStats (parsingStat Base) ast <*>
  parseAllStats (\x -> mapToN 0 . parsingStat EV' x) ast <*>
  parseAllStats (\x -> mapToN 1 . parsingStat IV' x) ast <*>
  parseMove1 ast <*>
  return (parseMove2 ast) <*>
  return (parseMove3 ast) <*>
  return (parseMove4 ast) <*>
  return (parseHeldItem ast) <*>
  parseAbility1 ast <*>
  return (parseAbility2 ast) <*>
  return (parseHiddenAbility ast) <*>
  parseWeight ast <*>
  parseHeight ast <*>
  return (Nature "Calm") <*>
  parseGender ast <*>
  return ()

mapToN :: Int -> Either String Int -> Either String Int
mapToN n (Left s) = Right n
mapToN n (Right s) = Right s
