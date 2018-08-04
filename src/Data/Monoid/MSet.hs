{-# LANGUAGE DeriveFunctor #-}
{- |
    Monoid and [group actions](https://en.wikipedia.org/wiki/Group_action) (M-Sets and G-Sets).
    The category of @MSet@s (and @GSet@s) is monadic (unlike the category of @SSet@s).
 -}
module Data.Monoid.MSet
    ( MSet
    , Endo (..)
    , rep
    , fact
    , FreeMSet (..)
    , hoistFreeMSet
    ) where

import           Control.Monad (ap)
import           Data.Constraint (Dict (..))
import           Data.Functor.Const (Const (..))
import           Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Product as Functor (Product)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Monoid (Monoid, Endo (..), Sum (..))
import           Data.Natural (Natural)
import           Data.Ord (Down)
import           Data.Set (Set)

import           Data.Semigroup.SSet (SSet (..), fact, rep)
import           Data.Algebra.Free (AlgebraType, AlgebraType0, FreeAlgebra (..), Proof (..), bindFree)

-- |
-- Lawful instance should satisfy:
--
-- prop> act mempty = id
-- prop> g `act` h `act` a = g <> h `act` a
--
-- This is the same as to say that `act` is a monoid homomorphism from @m@ to
-- the monoid of endomorphisms of @a@ (i.e. maps from @a@ to @a@).
--
-- Note that if @g@ is a @'Group'@ then an @MSet@ is simply a @GSet@, this
-- is because monoids and groups share the same morphisms (a monoid homomorphis
-- between groups necessarily preserves inverses).
class (Monoid m, SSet m a) => MSet m a

instance Monoid m => MSet m m

instance (MSet m a, MSet m b) => MSet m (a, b)

instance (MSet m a, MSet m b, MSet m c) => MSet m (a, b, c)

instance (MSet m a, MSet m b, MSet m c, MSet m d) => MSet m (a, b, c, d)

instance (MSet m a, MSet m b, MSet m c, MSet m d, MSet m e) => MSet m (a, b, c, d, e)

instance (MSet m a, MSet m b, MSet m c, MSet m d, MSet m e, MSet m f) => MSet m (a, b, c, d, e, f)

instance (MSet m a, MSet m b, MSet m c, MSet m d, MSet m e, MSet m f, MSet m h) => MSet m (a, b, c, d, e, f, h)

instance (MSet m a, MSet m b, MSet m c, MSet m d, MSet m e, MSet m f, MSet m h, MSet m i) => MSet m (a, b, c, d, e, f, h, i)

instance MSet m a => MSet m [a]

instance MSet m a => MSet m (NonEmpty a)

instance (MSet m a, Ord a) => MSet m (Set a)

{--
  - instance {-# OVERLAPPABLE #-} (Functor f, MSet m a) => MSet m (f a) where
  -     act m fa = fmap (act m) fa
  --}

instance MSet m a => MSet m (Identity a)

instance MSet m a => MSet (Identity m) a

instance MSet m a => MSet m (Maybe a)

instance MSet m b => MSet m (Either a b)

instance MSet m a => MSet m (Down a)

instance MSet m a => MSet m (IO a)

instance MSet m b => MSet m (a -> b)

instance MSet (Endo a) a

instance Monoid m => MSet (Sum Natural) m

instance MSet m a => MSet m (Const a b)

instance (Functor f, Functor h, MSet m a) => MSet m (Functor.Product f h a)

instance (Functor f, Functor h, MSet m a) => MSet m (Functor.Sum f h a)

newtype FreeMSet m a = FreeMSet { runFreeMSet :: (m, a) }
    deriving (Show, Ord, Eq, Functor)

hoistFreeMSet
    :: (m -> n)       -- ^ monoid homomorphism
    -> FreeMSet m a
    -> FreeMSet n a
hoistFreeMSet f (FreeMSet (m, a)) = FreeMSet (f m, a)

instance Monoid m => Applicative (FreeMSet m) where
    pure  = returnFree
    (<*>) = ap

instance Monoid m => Monad (FreeMSet m) where
    return = returnFree
    (>>=)  = bindFree

instance Semigroup m => SSet m (FreeMSet m a) where
    act m (FreeMSet (h, a)) = FreeMSet $ (m <> h, a)

instance Monoid m => MSet m (FreeMSet m a)

type instance AlgebraType0 (FreeMSet m) a = ()
type instance AlgebraType  (FreeMSet m) a = MSet m a
instance Monoid m => FreeAlgebra (FreeMSet m) where
    returnFree a = FreeMSet (mempty, a)
    foldMapFree f (FreeMSet (m, a)) = act m (f a)
    proof = Proof Dict
