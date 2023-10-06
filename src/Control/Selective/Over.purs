module Control.Selective.Over where

import Prelude

import Control.Select (class Select)
import Control.Selective (class Selective)
import Data.Newtype (class Newtype)

-- | Collects optional effects. See also `Under`.
newtype Over ∷ ∀ k. Type → k → Type
newtype Over m a = Over m

derive instance Newtype (Over m a) _

getOver ∷ ∀ @m @a. Over m a → m
getOver (Over m) = m

instance Functor (Over m) where
  map _ (Over a) = Over a

instance Semigroup m ⇒ Apply (Over m) where
  apply (Over a) (Over b) = Over (a <> b)

instance Semigroup m ⇒ Select (Over m) where
  select (Over a) (Over b) = Over (a <> b)

instance Monoid m ⇒ Applicative (Over m) where
  pure _ = Over mempty

instance Monoid m ⇒ Selective (Over m)
