module Control.Selective.ComposeTS where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, empty, (<|>))
import Control.Apply (lift2)
import Control.Select (class Select, (<*?))
import Control.Selective (class Selective)
import Data.Either (Either(..))
import Data.Eq (class Eq1, eq1)
import Data.Functor.App (hoistLiftApp)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1, compare1)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable, sequence)

-- | This newtype provides a `Select` instance for `Compose`.
-- | It "lifts" the `Traversable` `g` into the `Selective` `f`. `ComposeTS` does
-- | this in a way that works well when the `g`/inner type is like `Tuple` or
-- | `Maybe`, never containing more than one `a`. More details in the
-- | documentation for the `Select` instance. See `ComposeTM` for a different
-- | implementation that's better suited for arrays.
newtype ComposeTS ∷ ∀ k1 k2. (k2 → Type) → (k1 → k2) → k1 → Type
newtype ComposeTS f g a = ComposeTS (f (g a))

derive instance Newtype (ComposeTS f g a) _
derive instance Generic (ComposeTS f g a) _

instance Show (f (g a)) ⇒ Show (ComposeTS f g a) where
  show = genericShow

instance (Eq1 f, Eq1 g, Eq a) ⇒ Eq (ComposeTS f g a) where
  eq (ComposeTS a) (ComposeTS b) = eq1 (hoistLiftApp a) (hoistLiftApp b)

derive instance (Eq1 f, Eq1 g) ⇒ Eq1 (ComposeTS f g)

instance (Ord1 f, Ord1 g, Ord a) ⇒ Ord (ComposeTS f g a) where
  compare (ComposeTS a) (ComposeTS b) =
    compare1 (hoistLiftApp a) (hoistLiftApp b)

derive instance (Ord1 f, Ord1 g) ⇒ Ord1 (ComposeTS f g)

derive instance (Functor f, Functor g) ⇒ Functor (ComposeTS f g)

instance (Apply f, Apply g) ⇒ Apply (ComposeTS f g) where
  apply (ComposeTS f) (ComposeTS g) = ComposeTS (lift2 apply f g)

instance (Applicative f, Applicative g) ⇒ Applicative (ComposeTS f g) where
  pure x = ComposeTS (pure (pure x))

-- | This newtype provides a `Select` instance for `Compose`. It "lifts" the
-- | `Traversable` `g` into the `Selective` `f`.
-- |
-- | This instance works best when the traversable type never has more than one
-- | "slot", like `Maybe` or `Tuple`. If `g` does have more than one slot, like
-- | `Array`, one `Left` in the first argument to `select` makes it as if every
-- | element was that specific `Left`, replacing any `Right`s and subsequent
-- | `Left`s. See `ComposeTM` for an instance that works better with types like
-- | `Array`.
instance (Select f, Apply g, Traversable g) ⇒ Select (ComposeTS f g) where
  select (ComposeTS e) (ComposeTS f) = ComposeTS ((mapE <$> e) <*? (mapF <$> f))
    where
    mapE ∷ ∀ a b. g (Either a b) → Either (g a) (g b)
    mapE g = case sequence g of
      Left a → Left (g $> a)
      Right b → Right b

    mapF ∷ ∀ a b. g (a → b) → g a → g b
    mapF = flip (lift2 (#))

instance (Selective f, Applicative g, Traversable g) ⇒ Selective (ComposeTS f g)

instance (Alt f, Functor g) ⇒ Alt (ComposeTS f g) where
  alt (ComposeTS a) (ComposeTS b) = ComposeTS (a <|> b)

instance (Plus f, Functor g) ⇒ Plus (ComposeTS f g) where
  empty = ComposeTS empty

instance (Alternative f, Applicative g) ⇒ Alternative (ComposeTS f g)
