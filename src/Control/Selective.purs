module Control.Selective where

import Prelude

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Free (Free)
import Control.Monad.Identity.Trans (IdentityT)
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.ST (ST)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Control.Select (class Select, andS, orS)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)
import Data.Foldable (class Foldable, foldr)
import Data.Functor.Product (Product)
import Data.Identity (Identity)
import Data.Lazy as Data.Lazy
import Data.List.Lazy.Types as Lazy
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Run (Run)
import Uncurried.RWSET (RWSET) as Uncurried
import Uncurried.ReaderT (ReaderT) as Uncurried
import Uncurried.StateT (StateT) as Uncurried
import Uncurried.WriterT (WriterT) as Uncurried

{-
| This typeclass extends `Select` with an identity (`pure`).
| `Selective` is an abstraction that sits between `Applicative` and `Monad`.
|
| Instances must satisfy the following laws:
| - Identity: `x <*? pure identity = either identity identity <$> x`
| - Distributivity: `pure x <*? (y *> z) = (pure x <*? y) *> (pure x <*? z)`
| - Monad subclass (if `f` is a `Monad`): `select = selectM`
-}
class (Applicative f, Select f) ⇐ Selective f

instance Selective Array
instance Selective NonEmptyArray
instance Selective List
instance Selective NonEmptyList
instance Selective Lazy.List
instance Selective Lazy.NonEmptyList
instance Selective Maybe
instance Selective (Function r)
instance Selective (Either e)
instance Selective Effect
instance Selective Aff
instance Selective Identity
instance Selective (ST h)
instance Selective Data.Lazy.Lazy
instance Selective (Free f)
instance Selective (Run r)
instance Monoid w ⇒ Selective (Tuple w)
instance (Selective f, Selective g) ⇒ Selective (Product f g)
instance Selective f ⇒ Selective (IdentityT f)
instance Selective f ⇒ Selective (ReaderT r f)
instance (Selective f, Monoid w) ⇒ Selective (WriterT w f)
instance (Monad f, Monoid w) ⇒ Selective (RWST r w s f)
instance Applicative f ⇒ Selective (ContT r f)
instance Monad f ⇒ Selective (ExceptT e f)
instance Monad f ⇒ Selective (StateT s f)
instance Monad f ⇒ Selective (ListT f)
instance Monad f ⇒ Selective (MaybeT f)
instance Selective (Uncurried.ReaderT r f)
instance Selective (Uncurried.StateT s f)
instance Monoid w ⇒ Selective (Uncurried.WriterT w f)
instance Monoid w ⇒ Selective (Uncurried.RWSET r w s e f)

-- | Short-circuiting any
anyS ∷ ∀ @t @f @a. Selective f ⇒ Foldable t ⇒ (a → f Boolean) → t a → f Boolean
anyS pred = foldr (pred >>> orS) (pure false)

-- | Short-circuiting all
allS ∷ ∀ @t @f @a. Selective f ⇒ Foldable t ⇒ (a → f Boolean) → t a → f Boolean
allS pred = foldr (pred >>> andS) (pure true)
