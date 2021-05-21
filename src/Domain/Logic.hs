module Domain.Logic (
  not
  ,(~/)
  ,and
  ,or
  ,and'
  ,or'
  ,when
  ,(-->)
  ,othercase
  ,Action()
  ,notApplied
  ,applied
  ,is
  ,isn't
                    ) where

import Prelude hiding (not,or,and)
import qualified Prelude as Pre
import Attribute.Counterparty
import Control.Lens (isn't)
import Control.Lens.Extras (is)

not :: Functor m => m Bool -> m Bool
not m = Pre.not <$> m

(~/) :: Applicative m => (a -> m Bool) -> a -> m Bool
a ~/ b = not $ a b

infixl 5 ~/

and :: Applicative m => m Bool -> m Bool -> m Bool
a `and` b = (&&) <$> a <*> b

infixl 4 `and`

or :: Applicative m => m Bool -> m Bool -> m Bool
a `or` b = (||) <$> a <*> b

infixl 4 `or`


and' :: Applicative m => m Bool -> m Bool -> m Bool
a `and'` b = (&&) <$> a <*> b

infixl 3 `and'`

or' :: Applicative m => m Bool -> m Bool -> m Bool
a `or'` b = (||) <$> a <*> b

infixl 3 `or'`

when :: Applicative m => (m Bool -> m a) -> m Bool -> m a
when = ($)

infixr 0 `when`

data Action = NotApplied | Applied deriving (Eq,Show,Ord,Enum)

notApplied :: Applicative f => f Action
notApplied = pure NotApplied

applied :: Applicative f => f Action
applied = pure Applied

instance Semigroup Action where
  NotApplied <> NotApplied = NotApplied
  _ <> Applied = Applied
  Applied <> _ = Applied

instance Monoid Action where
  mempty = NotApplied

(-->) :: (Monoid a, Monad m) => m Bool -> m a -> m a
(-->) b a = do
  t <- b
  if t
    then a
    else return mempty

infixr 1 -->

othercase :: (Eq a, Monoid a, Monad m) => m a -> m a -> m a
othercase action alternative = do
  x <- action
  if x /= mempty
    then return x
    else alternative

infixl 0 `othercase`

