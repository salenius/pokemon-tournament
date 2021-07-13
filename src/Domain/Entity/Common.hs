{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Domain.Entity.Common where

import Prelude hiding ((.))
import Control.Applicative
import Control.Lens.Iso

data AST a =
  Branch a (AST a)
  | End
  deriving (Eq,Show,Ord,Functor,Foldable,Traversable)

instance Applicative AST where
  pure a = Branch a End
  Branch f b <*> Branch x c = Branch (f x) (b <*> c)
  Branch _ _ <*> End = End
  End <*> _ = End

instance Alternative AST where
  empty = End
  Branch a c <|> b = Branch a (c <|> b)
  End <|> b = b

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g a = f (g a)

infixr 8 .

astToList :: AST a -> [a]
astToList = foldr (:) []

astAsList :: Iso' (AST a) [a]
astAsList = iso astToList (foldr Branch End)

listAsAst :: Iso' [a] (AST a)
listAsAst = from astAsList

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither s m = case m of
  Just x -> Right x
  Nothing -> Left s
