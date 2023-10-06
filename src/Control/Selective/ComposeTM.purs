module Control.Selective.ComposeTM where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, empty, (<|>))
import Control.Apply (lift2)
import Control.Select (class Select, (<*?))
import Control.Selective (class Selective)
import Data.Eq (class Eq1, eq1)
import Data.Functor.App (hoistLiftApp)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1, compare1)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence)

-- | This newtype provides a `Select` instance for `Compose`.
-- | It "lifts" the `Traversable` `g` into the `Selective` `f`. `ComposeTM` does
-- | this in a way that works well when the `g`/inner type is like `Array` or
-- | `Maybe`, whose "effect" just involves the amount of elements, and doesn't
-- | store any extra data alongside elements, like `Tuple` does. More details
-- | in the documentation for the `Select` instance. See also `ComposeT` for an
-- | alternate implementation that's better suited for tuples.
newtype ComposeTM ∷ ∀ k1 k2. (k2 → Type) → (k1 → k2) → k1 → Type
newtype ComposeTM f g a = ComposeTM (f (g a))

derive instance Newtype (ComposeTM f g a) _
derive instance Generic (ComposeTM f g a) _

instance Show (f (g a)) ⇒ Show (ComposeTM f g a) where
  show = genericShow

instance (Eq1 f, Eq1 g, Eq a) ⇒ Eq (ComposeTM f g a) where
  eq (ComposeTM a) (ComposeTM b) = eq1 (hoistLiftApp a) (hoistLiftApp b)

derive instance (Eq1 f, Eq1 g) ⇒ Eq1 (ComposeTM f g)

instance (Ord1 f, Ord1 g, Ord a) ⇒ Ord (ComposeTM f g a) where
  compare (ComposeTM a) (ComposeTM b) =
    compare1 (hoistLiftApp a) (hoistLiftApp b)

derive instance (Ord1 f, Ord1 g) ⇒ Ord1 (ComposeTM f g)

derive instance (Functor f, Functor g) ⇒ Functor (ComposeTM f g)

instance (Apply f, Apply g) ⇒ Apply (ComposeTM f g) where
  apply (ComposeTM f) (ComposeTM g) = ComposeTM (lift2 apply f g)

instance (Applicative f, Applicative g) ⇒ Applicative (ComposeTM f g) where
  pure x = ComposeTM (pure (pure x))

-- | This newtype provides a `Select` instance for `Compose`. It "lifts" the
-- | `Traversable` `g` into the `Selective` `f`.
-- |
-- | This instance works best when the traversable's "effect" is
-- | failure/nondeterminism, like `Array` or `Maybe`. It doesn't work too well
-- | in other cases, for example if `g` is `Tuple`, then if the first parameter
-- | contains a `Left` (requiring use of the second parameter), *only* the
-- | second parameter's `fst` value actually ends up in the result. See
-- | `ComposeT` for an instance that works better for types like `Tuple`.
instance (Select f, Apply g, Traversable g) ⇒ Select (ComposeTM f g) where
  select (ComposeTM e) (ComposeTM f) = ComposeTM $
    (sequence <$> e) <*? (flap <$> f)

instance (Selective f, Applicative g, Traversable g) ⇒ Selective (ComposeTM f g)

instance (Alt f, Functor g) ⇒ Alt (ComposeTM f g) where
  alt (ComposeTM a) (ComposeTM b) = ComposeTM (a <|> b)

instance (Plus f, Functor g) ⇒ Plus (ComposeTM f g) where
  empty = ComposeTM empty

instance (Alternative f, Applicative g) ⇒ Alternative (ComposeTM f g)
