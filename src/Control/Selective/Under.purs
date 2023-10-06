module Control.Selective.Under where

import Prelude

import Control.Select (class Select)
import Control.Selective (class Selective)
import Data.Newtype (class Newtype)

-- | Ignores optional effects. See also `Over`.
newtype Under ∷ ∀ k. Type → k → Type
newtype Under m a = Under m

derive instance Newtype (Under m a) _

getUnder ∷ ∀ @m @a. Under m a → m
getUnder (Under m) = m

instance Functor (Under m) where
  map _ (Under a) = Under a

instance Semigroup m ⇒ Apply (Under m) where
  apply (Under a) (Under b) = Under (a <> b)

instance Semigroup m ⇒ Select (Under m) where
  select (Under a) _ = Under a

instance Monoid m ⇒ Applicative (Under m) where
  pure _ = Under mempty

instance Monoid m ⇒ Selective (Under m)
