module Data.Monoid.Action
    ( Action (..)
    , Endo (..)
    , rep
    , fact
    , FreeAction (..)
    , hoistFreeAction
    ) where

import           Control.Monad (ap)
import           Data.Monoid (Monoid, Endo (..), Sum (..))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Functor.Const (Const (..))
import           Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Product as Functor (Product)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.Group (Group (..))
import           Data.Natural (Natural)
import           Data.Ord (Down)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Algebra.Free (AlgebraType, FreeAlgebra (..), bindFree)

-- |
-- Lawful instance should satisfy:
-- @
--    act 'mempty' = 'id'
--    g `act` h `act` a = g <> h `act` a
-- @
-- This is the same as to say that `act` is a monoid homomorphism from @G@ to
-- the monoid of endomorphisms of @a@ (i.e. maps from @a@ to @a@).
--
-- Note that if @g@ is a @'Group'@ then @'Action' g@ is simply a @GSet@, this
-- is because monoids and groups share the same morphisms (a monoid homomorphis
-- between groups necessarily preserves inverses).
class Monoid m => Action m a where
    act :: m -> a -> a

-- |
-- There is a 1-1 correspondence between actions and representations of @m@ in
-- @'Endo' a@.  
rep :: Action m a => m -> Endo a
rep m = Endo (act m)

instance Monoid m => Action m m where
    act = (<>)

instance (Action m a, Action m b) => Action m (a, b) where
    act m (a, b) = (act m a, act m b)

instance (Action m a, Action m b, Action m c) => Action m (a, b, c) where
    act m (a, b, c) = (act m a, act m b, act m c)

instance (Action m a, Action m b, Action m c, Action m d) => Action m (a, b, c, d) where
    act m (a, b, c, d) = (act m a, act m b, act m c, act m d)

instance (Action m a, Action m b, Action m c, Action m d, Action m e) => Action m (a, b, c, d, e) where
    act m (a, b, c, d, e) = (act m a, act m b, act m c, act m d, act m e)

instance (Action m a, Action m b, Action m c, Action m d, Action m e, Action m f) => Action m (a, b, c, d, e, f) where
    act m (a, b, c, d, e, f) = (act m a, act m b, act m c, act m d, act m e, act m f)

instance (Action m a, Action m b, Action m c, Action m d, Action m e, Action m f, Action m h) => Action m (a, b, c, d, e, f, h) where
    act m (a, b, c, d, e, f, h) = (act m a, act m b, act m c, act m d, act m e, act m f, act m h)

instance (Action m a, Action m b, Action m c, Action m d, Action m e, Action m f, Action m h, Action m i) => Action m (a, b, c, d, e, f, h, i) where
    act m (a, b, c, d, e, f, h, i) = (act m a, act m b, act m c, act m d, act m e, act m f, act m h, act m i)

instance Action m a => Action m [a] where
    act m as = map (act m) as

instance Action m a => Action m (NonEmpty a) where
    act m as = NE.map (act m) as

instance (Action m a, Ord a) => Action m (Set a) where
    act m as = Set.map (act m) as

-- |
-- Any @'Action'@ wrapped in a functor is a valid @'Action'@.
fact :: (Functor f, Action m a) => m -> f a -> f a
fact m = fmap (act m)

{--
  - instance {-# OVERLAPPABLE #-} (Functor f, Action m a) => Action m (f a) where
  -     act m fa = fmap (act m) fa
  --}

instance Action m a => Action m (Identity a) where
    act = fact

instance Action m a => Action (Identity m) a where
    act (Identity f) a = f `act` a

instance Action m a => Action m (Maybe a) where
    act = fact

instance Action m b => Action m (Either a b) where
    act = fact

instance Action m a => Action m (Down a) where
    act = fact

instance Action m a => Action m (IO a) where
    act = fact

instance Action m b => Action m (a -> b) where
    act = fact

instance Action (Endo a) a where
    act (Endo f) a = f a

instance Monoid m => Action (Sum Natural) m where
    act (Sum 0) _ = mempty
    act (Sum n) m = m <> act (Sum (n - 1)) m

instance Group g => Action (Sum Integer) g where
    act (Sum n) g | n < 0      = invert g <> act (Sum (n + 1)) g
                  | n > 0      = g <> act (Sum (n - 1)) g
                  | otherwise  = mempty

instance Action m a => Action m (Const a b) where
    act m (Const a) = Const $ m `act` a

instance (Functor f, Functor h, Action m a) => Action m (Functor.Product f h a) where
    act = fact

instance (Functor f, Functor h, Action m a) => Action m (Functor.Sum f h a) where
    act = fact

newtype FreeAction m a = FreeAction { runFreeAction :: (m, a) }
    deriving (Show, Ord, Eq, Functor)

hoistFreeAction
    :: (m -> n)       -- ^ monoid homomorphism
    -> FreeAction m a
    -> FreeAction n a
hoistFreeAction f (FreeAction (m, a)) = FreeAction (f m, a)

instance Monoid m => Applicative (FreeAction m) where
    pure  = returnFree
    (<*>) = ap

instance Monoid m => Monad (FreeAction m) where
    return = returnFree
    (>>=)  = bindFree

instance Monoid m => Action m (FreeAction m a) where
    act m (FreeAction (h, a)) = FreeAction $ (m <> h, a)

type instance AlgebraType (FreeAction m) a = Action m a
instance Monoid m => FreeAlgebra (FreeAction m) where
    returnFree a = FreeAction (mempty, a)
    foldMapFree f (FreeAction (m, a)) = act m (f a)
