module Control.Selective.SelectM where

import Prelude

import Control.Select (class Select, selectM)
import Control.Selective (class Selective)
import Data.Newtype (class Newtype)

-- | Has a `Select` instance where `select` is implemented in terms of `bind`
newtype SelectM ∷ ∀ k. (k → Type) → k → Type
newtype SelectM m a = SelectM (m a)

derive instance Newtype (SelectM m a) _

derive newtype instance Functor m ⇒ Functor (SelectM m)
derive newtype instance Apply m ⇒ Apply (SelectM m)
derive newtype instance Applicative m ⇒ Applicative (SelectM m)

instance Monad m ⇒ Select (SelectM m) where
  select (SelectM e) (SelectM f) = SelectM (e `selectM` f)

instance Monad m ⇒ Selective (SelectM m)

derive newtype instance Bind m ⇒ Bind (SelectM m)
derive newtype instance Monad m ⇒ Monad (SelectM m)
