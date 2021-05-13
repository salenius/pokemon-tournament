{-# LANGUAGE RankNTypes #-}


module Attribute.Env (
  module Appl
  ,module Pri
  ,Parser
  ,parse
  ,parserToMaybe
  ,Counterparty(..)
  ,Player(..)
  ,mkPrism
                     ) where

import Control.Applicative as Appl
import Control.Monad.Reader
import Control.Lens as Pri

type Parser r a = ReaderT r Maybe a

parse :: Prism' d a -> Parser d a
parse prsim = do
  r  <- ask
  r' <- lift $ preview prsim r
  return r'

parserToMaybe :: Parser d a -> d -> Maybe a
parserToMaybe prsr env = runReaderT prsr env

data Counterparty =
  User
  | Target
  deriving (Eq,Show,Read,Enum,Ord)

data Player =
  Player1
  | Player2
  deriving (Eq,Show,Read,Enum,Ord)

mkPrism :: (Read a, Show a) => Prism' String a
mkPrism = prism' show safeRead
  where
    safeRead s
      | [(x, "")] <- reads s = Just x
      | otherwise = Nothing
