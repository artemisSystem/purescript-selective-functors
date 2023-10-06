module Control.Selective.SelectTM where

import Prelude

import Control.Select (class Select, selectTM)
import Control.Selective (class Selective)
import Data.Newtype (class Newtype)
import Data.Traversable (class Foldable, class Traversable)

-- | Has a `Select` instance where `select` is implemented in terms of
-- | `sequence`. See also `SelectTS`.
newtype SelectTM ∷ ∀ k. (k → Type) → k → Type
newtype SelectTM t a = SelectTM (t a)

derive instance Newtype (SelectTM t a) _

derive newtype instance Functor t ⇒ Functor (SelectTM t)
derive newtype instance Apply t ⇒ Apply (SelectTM t)
derive newtype instance Applicative t ⇒ Applicative (SelectTM t)

-- | `Select` implemented in terms of `sequence`. With this implementation,
-- | depending on the `Either` in the first argument, the first or the second
-- | effect will be executed, but never both. `Traversable` allows us to
-- | inspect the `Either` in the first parameter without including its effect
-- | in the result. This implementation is well suited for array-like
-- | traversables. (i.e. potentially containing more than one element).
instance (Traversable t, Apply t) ⇒ Select (SelectTM t) where
  select (SelectTM e) (SelectTM f) = SelectTM (e `selectTM` f)

instance (Traversable t, Applicative t) ⇒ Selective (SelectTM t)

derive newtype instance Foldable t ⇒ Foldable (SelectTM t)
derive newtype instance Traversable t ⇒ Traversable (SelectTM t)
