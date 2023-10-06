module Control.Select where

import Prelude

import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Monad.Cont (ContT(..))
import Control.Monad.Except (ExceptT)
import Control.Monad.Free (Free)
import Control.Monad.Identity.Trans (IdentityT(..))
import Control.Monad.List.Trans (ListT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.ST (ST)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT(..))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..), note)
import Data.Functor.Product (Product(..))
import Data.Identity (Identity)
import Data.Lazy as Data.Lazy
import Data.List.Lazy.Types as Lazy
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe)
import Data.Semigroup.Foldable (class Foldable1, foldr1)
import Data.Traversable (class Traversable, sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Run (Run)
import Uncurried.RWSET (RWSET) as Uncurried
import Uncurried.ReaderT (ReaderT) as Uncurried
import Uncurried.StateT (StateT) as Uncurried
import Uncurried.WriterT (WriterT) as Uncurried

{-
| `Select` is an abstraction that sits between `Apply` and `Bind`.
|
| Instances must satisfy the following law:
| - Associativity:
| ```purescript
|   x <*? (y <*? z) = ((map Right <$> x) <*? (f <$> y)) <*? uncurry z
|   where
|   f (Left y) x = Tuple y x
|   f (Right y) x = y x
| ```
-}
class Apply f ⇐ Select f where
  select ∷ ∀ a b. f (Either a b) → f (a → b) → f b

instance Select Array where
  select = selectM

instance Select NonEmptyArray where
  select = selectM

instance Select List where
  select = selectM

instance Select NonEmptyList where
  select = selectM

instance Select Lazy.List where
  select = selectM

instance Select Lazy.NonEmptyList where
  select = selectM

instance Select Maybe where
  select = selectM

instance Select (Function r) where
  select = selectM

instance Select (Either e) where
  select = selectM

instance Select Effect where
  select = selectM

instance Select Aff where
  select = selectM

instance Select Identity where
  select = selectM

instance Select (ST h) where
  select = selectM

instance Select Data.Lazy.Lazy where
  select = selectM

instance Select (Free f) where
  select = selectM

instance Select (Run r) where
  select = selectM

instance Semigroup w ⇒ Select (Tuple w) where
  select (Tuple w1 e) (Tuple w2 f) = case e of
    Left a → Tuple (w1 <> w2) (f a)
    Right b → Tuple w1 b

instance (Select f, Select g) ⇒ Select (Product f g) where
  select (Product (Tuple f1 g1)) (Product (Tuple f2 g2)) =
    Product (Tuple (f1 <*? f2) (g1 <*? g2))

instance Select f ⇒ Select (IdentityT f) where
  select (IdentityT e) (IdentityT f) = IdentityT (e <*? f)

instance Select f ⇒ Select (ReaderT r f) where
  select (ReaderT e) (ReaderT f) = ReaderT \r → e r <*? f r

instance (Select f, Semigroup w) ⇒ Select (WriterT w f) where
  select (WriterT e) (WriterT f) = WriterT ((mapE <$> e) <*? (mapF <$> f))
    where
    mapE ∷ ∀ a b. Tuple (Either a b) w → Either (Tuple a w) (Tuple b w)
    mapE (Tuple (Left a) w) = Left (Tuple a w)
    mapE (Tuple (Right b) w) = Right (Tuple b w)

    mapF ∷ ∀ a b. Tuple (a → b) w → Tuple a w → Tuple b w
    mapF (Tuple innerF w2) (Tuple a w1) = Tuple (innerF a) (w1 <> w2)

instance (Monad f, Monoid w) ⇒ Select (RWST r w s f) where
  select = selectM

instance Apply f ⇒ Select (ContT r f) where
  select (ContT e) (ContT f) = ContT \res → e case _ of
    Left a → f \func → res (func a)
    Right b → res b

-- | This instance requires the inner type to be a `Monad`, because the `Apply`
-- | instance requires it.
instance Monad f ⇒ Select (ExceptT e f) where
  select = selectM

-- | This instance requires the inner type to be a `Monad`, because the `Apply`
-- | instance requires it.
instance Monad f ⇒ Select (StateT s f) where
  select = selectM

-- | This instance requires the inner type to be a `Monad`, because the `Apply`
-- | instance requires it.
instance Monad f ⇒ Select (ListT f) where
  select = selectM

-- | This instance requires the inner type to be a `Monad`, because the `Apply`
-- | instance requires it.
instance Monad f ⇒ Select (MaybeT f) where
  select = selectM

instance Select (Uncurried.ReaderT r f) where
  select = selectM

instance Select (Uncurried.StateT s f) where
  select = selectM

instance Monoid w ⇒ Select (Uncurried.WriterT w f) where
  select = selectM

instance Monoid w ⇒ Select (Uncurried.RWSET r w s e f) where
  select = selectM

infixl 4 select as <*?

-- | `select` implemented in terms of `bind` and `pure`
selectM ∷ ∀ @m @a @b. Monad m ⇒ m (Either a b) → m (a → b) → m b
selectM e f = e >>= case _ of
  Left a → (_ $ a) <$> f
  Right b → pure b

-- | `select` implemented in terms of `sequence`. With this implementation,
-- | depending on the `Either` in the first argument, the first or the second
-- | effect will be executed, but never both. `Traversable` allows us to
-- | inspect the `Either` in the first parameter without including its effect
-- | in the result. This implementation is well suited for array-like
-- | traversables. (i.e. potentially containing more than one element)
selectTM ∷ ∀ @t @a @b. Traversable t ⇒ t (Either a b) → t (a → b) → t b
selectTM e f = case sequence e of
  Left a → (_ $ a) <$> f
  Right tb → tb

-- | `select` implemented in terms of `sequence` and `apply`. This
-- | implementation uses `apply` to keep the effects of the first parameter
-- | when it is a `Left`, unlike `selectTM`. This implementation is well suited
-- | for tuple-like traversables (i.e. containing one or fewer elements)
selectTS
  ∷ ∀ @f @a @b. Traversable f ⇒ Apply f ⇒ f (Either a b) → f (a → b) → f b
selectTS e f = case sequence e of
  Left a → (_ $ a) <$ e <*> f
  Right tb → tb

-- | `select` implemented in terms of `apply` (both effects are always executed)
selectA ∷ ∀ @f @a @b. Apply f ⇒ f (Either a b) → f (a → b) → f b
selectA = lift2 case _ of
  Left a → \f → f a
  Right tb → \_ → tb

-- | `apply` implemented in terms of `select`
applyS ∷ ∀ @f @a @b. Select f ⇒ f (a → b) → f a → f b
applyS f x = select (Left <$> f) ((#) <$> x)

-- | Execute the second action if the first action returns true
whenS ∷ ∀ @f. Select f ⇒ f Boolean → f Unit → f Unit
whenS cond f = select
  (cond <#> if _ then Left unit else Right unit)
  (f $> identity)

-- | Execute the second action if the first action returns false
unlessS ∷ ∀ @f. Select f ⇒ f Boolean → f Unit → f Unit
unlessS cond = whenS (not <$> cond)

-- | Execute either the second or the third action, depending on the result of
-- | the first
branch ∷ ∀ @f @a @b @c. Select f ⇒ f (Either a b) → f (a → c) → f (b → c) → f c
branch cond l r = (map Left <$> cond) <*? (map Right <$> l) <*? r

-- | If the first action is true, execute the second action, otherwise the third
ifS ∷ ∀ @f @a. Select f ⇒ f Boolean → f a → f a → f a
ifS cond a b = branch
  (cond <#> if _ then Left unit else Right unit)
  (const <$> a)
  (const <$> b)

-- | Short-circuiting or
orS ∷ ∀ @f. Select f ⇒ f Boolean → f Boolean → f Boolean
orS x y = (x <#> if _ then Right true else Left unit) <*? (const <$> y)

infixr 2 orS as <||>

-- | Short-circuiting and
andS ∷ ∀ @f. Select f ⇒ f Boolean → f Boolean → f Boolean
andS x y = (x <#> if _ then Left unit else Right false) <*? (const <$> y)

infixr 3 andS as <&&>

-- | Short-circuiting any
any1S ∷ ∀ @t @f. Select f ⇒ Foldable1 t ⇒ t (f Boolean) → f Boolean
any1S = foldr1 orS

-- | Short-circuiting all
all1S ∷ ∀ @t @f. Select f ⇒ Foldable1 t ⇒ t (f Boolean) → f Boolean
all1S = foldr1 andS

-- | If the second action is `Nothing`, run and return the first
fromMaybeS ∷ ∀ @f @a. Select f ⇒ f a → f (Maybe a) → f a
fromMaybeS default f = (note unit <$> f) <*? (const <$> default)

-- | Repeat an action until it returns true
whileS ∷ ∀ @f. Lazy (f Unit) ⇒ Select f ⇒ f Boolean → f Unit
whileS f = whenS f (defer \_ → whileS f)
