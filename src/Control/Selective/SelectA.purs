module Control.Selective.SelectA where

import Prelude

import Control.Select (class Select, selectA)
import Control.Selective (class Selective)
import Data.Newtype (class Newtype)

-- | Has a `Select` instance where `select` is implemented in terms of `apply`
newtype SelectA ∷ ∀ k. (k → Type) → k → Type
newtype SelectA f a = SelectA (f a)

derive instance Newtype (SelectA f a) _

derive newtype instance Functor f ⇒ Functor (SelectA f)
derive newtype instance Apply f ⇒ Apply (SelectA f)
derive newtype instance Applicative f ⇒ Applicative (SelectA f)

instance Apply f ⇒ Select (SelectA f) where
  select (SelectA e) (SelectA f) = SelectA (e `selectA` f)

instance Applicative f ⇒ Selective (SelectA f)
