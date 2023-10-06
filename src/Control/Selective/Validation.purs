module Control.Selective.Validation where

import Prelude

import Control.Select (class Select)
import Control.Selective (class Selective)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | Ignores errors in not-chosen branches.
data Validation e a = Failure e | Success a

derive instance Generic (Validation e a) _

instance (Show e, Show a) ⇒ Show (Validation e a) where
  show = genericShow

derive instance Functor (Validation e)

instance Semigroup e ⇒ Apply (Validation e) where
  apply (Failure e1) (Failure e2) = Failure (e1 <> e2)
  apply (Failure e1) (Success _) = Failure e1
  apply (Success _) (Failure e2) = Failure e2
  apply (Success f) (Success a) = Success (f a)

instance Semigroup e ⇒ Select (Validation e) where
  select (Success (Left a)) f = (_ $ a) <$> f
  select (Success (Right b)) _ = Success b
  select (Failure e) _ = Failure e

instance Semigroup e ⇒ Applicative (Validation e) where
  pure = Success

instance Semigroup e ⇒ Selective (Validation e)
