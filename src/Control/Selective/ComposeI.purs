module Control.Selective.ComposeI where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus, empty, (<|>))
import Control.Apply (lift2)
import Control.Select (class Select, select)
import Control.Selective (class Selective)
import Data.Eq (class Eq1, eq1)
import Data.Functor.App (hoistLiftApp)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord1, compare1)
import Data.Show.Generic (genericShow)

-- | This newtype provides a `Select` instance for `Compose`.
-- | It "lifts" the `Selective` `g` into the `Applicative` `f`. In other words,
-- | it composes an outer `Applicative` and an inner `Selective`.
newtype ComposeI ∷ ∀ k1 k2. (k2 → Type) → (k1 → k2) → k1 → Type
newtype ComposeI f g a = ComposeI (f (g a))

derive instance Newtype (ComposeI f g a) _
derive instance Generic (ComposeI f g a) _

instance Show (f (g a)) ⇒ Show (ComposeI f g a) where
  show = genericShow

instance (Eq1 f, Eq1 g, Eq a) ⇒ Eq (ComposeI f g a) where
  eq (ComposeI a) (ComposeI b) = eq1 (hoistLiftApp a) (hoistLiftApp b)

derive instance (Eq1 f, Eq1 g) ⇒ Eq1 (ComposeI f g)

instance (Ord1 f, Ord1 g, Ord a) ⇒ Ord (ComposeI f g a) where
  compare (ComposeI a) (ComposeI b) = compare1 (hoistLiftApp a) (hoistLiftApp b)

derive instance (Ord1 f, Ord1 g) ⇒ Ord1 (ComposeI f g)

derive instance (Functor f, Functor g) ⇒ Functor (ComposeI f g)

instance (Apply f, Apply g) ⇒ Apply (ComposeI f g) where
  apply (ComposeI f) (ComposeI g) = ComposeI (lift2 apply f g)

instance (Applicative f, Applicative g) ⇒ Applicative (ComposeI f g) where
  pure x = ComposeI (pure (pure x))

instance (Alt f, Functor g) ⇒ Alt (ComposeI f g) where
  alt (ComposeI a) (ComposeI b) = ComposeI (a <|> b)

instance (Plus f, Functor g) ⇒ Plus (ComposeI f g) where
  empty = ComposeI empty

instance (Alternative f, Applicative g) ⇒ Alternative (ComposeI f g)

-- | "Lifts" the inner type's selective instance into the outer via `lift2`.
-- | The outer effect will always be executed.
instance (Apply f, Select g) ⇒ Select (ComposeI f g) where
  select (ComposeI e) (ComposeI f) = ComposeI (lift2 select e f)

instance (Applicative f, Selective g) ⇒ Selective (ComposeI f g)
