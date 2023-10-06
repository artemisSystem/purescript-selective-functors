module Control.Selective.SelectTS where

import Prelude

import Control.Select (class Select, selectTS)
import Control.Selective (class Selective)
import Data.Newtype (class Newtype)
import Data.Traversable (class Foldable, class Traversable)

-- | Has a `Select` instance where `select` is implemented in terms of
-- | `sequence`. See also `SelectTM`
newtype SelectTS ∷ ∀ k. (k → Type) → k → Type
newtype SelectTS t a = SelectTS (t a)

derive instance Newtype (SelectTS t a) _

derive newtype instance Functor t ⇒ Functor (SelectTS t)
derive newtype instance Apply t ⇒ Apply (SelectTS t)
derive newtype instance Applicative t ⇒ Applicative (SelectTS t)

-- | `Select` implemented in terms of `sequence` and `apply`. This
-- | implementation uses `apply` to keep the effects of the first parameter
-- | when it is a `Left`, unlike `SelectTM`. This implementation is well suited
-- | for tuple-like traversables (i.e. containing one or fewer elements)
instance (Traversable t, Apply t) ⇒ Select (SelectTS t) where
  select (SelectTS e) (SelectTS f) = SelectTS (e `selectTS` f)

instance (Traversable t, Applicative t) ⇒ Selective (SelectTS t)

derive newtype instance Foldable t ⇒ Foldable (SelectTS t)
derive newtype instance Traversable t ⇒ Traversable (SelectTS t)
