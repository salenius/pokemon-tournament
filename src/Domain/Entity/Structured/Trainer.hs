{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}



module Domain.Entity.Structured.Trainer where

import Domain.Entity.GenV.Trainer as Tr

import Domain.Entity.Common hiding ((.))
import qualified Domain.Entity.Trainer as T
import Domain.Entity.Pokemon (unPokemon)
import qualified Domain.Entity.Structured.Pokemon as P
import Control.Lens

data Trainer pokemon = Trainer
  {
    _trainerName :: String
  , _pokemon1 :: pokemon
  , _pokemon2 :: Maybe pokemon
  , _pokemon3 :: Maybe pokemon
  , _pokemon4 :: Maybe pokemon
  , _pokemon5 :: Maybe pokemon
  , _pokemon6 :: Maybe pokemon
  } deriving (Eq,Show,Ord)

makeLenses ''Trainer

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

---

parseName = unambiguous (\case {T.Name n -> True; _ -> False}) (\(T.Name n) -> n) "Trainer name can only appear once."

parsePokemon1 = compress' . unambiguous (\case {T.Pokemon1 n -> True; _ -> False}) (\(T.Pokemon1 n) -> P.parsePokemon $ unPokemon n) "Pokemon name can only appear once."

parsePokemon2 = compress . eitherToMaybe . unambiguous (\case {T.Pokemon2 n -> True; _ -> False}) (\(T.Pokemon2 n) -> P.parsePokemon $ unPokemon n) "Pokemon name can only appear once."

parsePokemon3 = compress . eitherToMaybe . unambiguous (\case {T.Pokemon3 n -> True; _ -> False}) (\(T.Pokemon3 n) -> P.parsePokemon $ unPokemon n) "Pokemon name can only appear once."

parsePokemon4 = compress . eitherToMaybe . unambiguous (\case {T.Pokemon4 n -> True; _ -> False}) (\(T.Pokemon4 n) -> P.parsePokemon $ unPokemon n) "Pokemon name can only appear once."

parsePokemon5 = compress . eitherToMaybe . unambiguous (\case {T.Pokemon5 n -> True; _ -> False}) (\(T.Pokemon5 n) -> P.parsePokemon $ unPokemon n) "Pokemon name can only appear once."

parsePokemon6 = compress . eitherToMaybe . unambiguous (\case {T.Pokemon6 n -> True; _ -> False}) (\(T.Pokemon6 n) -> P.parsePokemon $ unPokemon n) "Pokemon name can only appear once."

parseTrainer ast =
  Trainer <$>
  parseName ast <*>
  parsePokemon1 ast <*>
  return (parsePokemon2 ast) <*>
  return (parsePokemon3 ast) <*>
  return (parsePokemon4 ast) <*>
  return (parsePokemon5 ast) <*>
  return (parsePokemon6 ast)
