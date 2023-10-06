module Control.Selective.Free where

import Prelude

import Control.Select (class Select, (<*?))
import Control.Selective (class Selective)
import Control.Selective.Over (Over(..))
import Control.Selective.SelectA (SelectA(..))
import Control.Selective.SelectM (SelectM(..))
import Control.Selective.SelectTM (SelectTM(..))
import Control.Selective.SelectTS (SelectTS(..))
import Control.Selective.Under (Under(..))
import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (un)
import Data.Traversable (class Traversable)

-- | A simple free `Selective`.
newtype FreeS f a = FreeS (∀ g. Selective g ⇒ (f ~> g) → g a)

instance Functor (FreeS f) where
  map f (FreeS free) = FreeS \nat → f <$> free nat

instance Apply (FreeS f) where
  apply (FreeS freeF) (FreeS freeA) = FreeS \nat → freeF nat <*> freeA nat

instance Select (FreeS f) where
  select (FreeS freeE) (FreeS freeF) = FreeS \nat → freeE nat <*? freeF nat

instance Applicative (FreeS f) where
  pure x = FreeS \_ → pure x

instance Selective (FreeS f)

-- | Lift a computation into the free `Selective`.
liftF ∷ ∀ @f. f ~> FreeS f
liftF f = FreeS \nat → nat f

-- | Run a free computation.
runFree ∷ ∀ @g @f. Selective g ⇒ (f ~> g) → (FreeS f ~> g)
runFree nat (FreeS free) = free nat

-- | Run a free computation in an applicative via `SelectA`
runFreeA ∷ ∀ @g @f. Applicative g ⇒ (f ~> g) → (FreeS f ~> g)
runFreeA nat = un SelectA <<< runFree (SelectA <<< nat)

-- | Run a free computation in a monad via `SelectM`
runFreeM ∷ ∀ @g @f. Monad g ⇒ (f ~> g) → (FreeS f ~> g)
runFreeM nat = un SelectM <<< runFree (SelectM <<< nat)

-- | Run a free computation in a traversable via `SelectTS`
runFreeTS ∷ ∀ @g @f. Traversable g ⇒ Applicative g ⇒ (f ~> g) → (FreeS f ~> g)
runFreeTS nat = un SelectTS <<< runFree (SelectTS <<< nat)

-- | Run a free computation in a traversable via `SelectTM`
runFreeTM ∷ ∀ @g @f. Traversable g ⇒ Applicative g ⇒ (f ~> g) → (FreeS f ~> g)
runFreeTM nat = un SelectTM <<< runFree (SelectTM <<< nat)

-- | Fold together every possible effect.
foldOver ∷ ∀ @m @f @a. Monoid m ⇒ (∀ x. f x → m) → FreeS f a → m
foldOver f = un Over <<< runFree (Over <<< f)

-- | Collect every possible effect into an `Array`.
effectsOver ∷ ∀ @f @a. Functor f ⇒ FreeS f a → Array (f Unit)
effectsOver = foldOver (singleton <<< void)

-- | Count how many possible effects there are.
countOver ∷ ∀ @f @a. FreeS f a → Int
countOver = un Additive <<< foldOver \_ → Additive 1

-- | Fold together every required effect.
foldUnder ∷ ∀ @m @f @a. Monoid m ⇒ (∀ x. f x → m) → FreeS f a → m
foldUnder f = un Under <<< runFree (Under <<< f)

-- | Collect every required effect into an `Array`.
effectsUnder ∷ ∀ @f @a. Functor f ⇒ FreeS f a → Array (f Unit)
effectsUnder = foldUnder (singleton <<< void)

-- | Count how many required effects there are.
countUnder ∷ ∀ @f @a. FreeS f a → Int
countUnder = un Additive <<< foldUnder \_ → Additive 1

-- | Extract a pure value if there are no effects.
getPure ∷ ∀ @a @f. FreeS f a → Maybe a
getPure (FreeS free) = free (const Nothing)
