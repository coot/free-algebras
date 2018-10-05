{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveFunctor #-}
{- |
    Monoid and [group actions](https://en.wikipedia.org/wiki/Group_action) (M-Sets and G-Sets).
    The category of @MSet@s (and @GSet@s) is monadic (unlike the category of @SSet@s).
 -}
module Data.Monoid.MSet
    ( MSet
    , SSet (..)
    , Endo (..)
    , rep
    , fact
    , FreeMSet (..)
    , hoistFreeMSet
    , foldrMSet
    , S (..)
    ) where

import           Control.Monad (ap)
import           Data.Functor.Const (Const (..))
import           Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Product as Functor (Product)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Monoid (Monoid, Endo (..), Sum (..))
import           Data.Natural (Natural)
import           Data.Ord (Down (..))
import           Data.Semigroup (Semigroup (..))
import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Semigroup.SSet (SSet (..), S (..), fact, rep)
import           Data.Algebra.Free
    ( AlgebraType
    , AlgebraType0
    , FreeAlgebra (..)
    , proof
    , bindFree
    , foldrFree
    )

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
#if __GLASGOW_HASKELL__ > 822
class (Monoid m , SSet m a) => MSet m a
  mact :: m -> a -> a
  mact = act
#else
class Monoid m => MSet m a where
  mact :: m -> a -> a
#endif

instance Monoid m => MSet m m where
#if __GLASGOW_HASKELL__ <= 822
  mact = mappend
#endif

instance (MSet m a, MSet m b) => MSet m (a, b) where
#if __GLASGOW_HASKELL__ <= 822
  mact m (a, b) = (mact m a, mact m b)
#endif

instance (MSet m a, MSet m b, MSet m c) => MSet m (a, b, c) where
#if __GLASGOW_HASKELL__ <= 822
    mact m (a, b, c) = (mact m a, mact m b, mact m c)
#endif

instance (MSet m a, MSet m b, MSet m c, MSet m d) => MSet m (a, b, c, d) where
#if __GLASGOW_HASKELL__ <= 822
    mact m (a, b, c, d) = (mact m a, mact m b, mact m c, mact m d)
#endif

instance (MSet m a, MSet m b, MSet m c, MSet m d, MSet m e) => MSet m (a, b, c, d, e) where
#if __GLASGOW_HASKELL__ <= 822
    mact m (a, b, c, d, e) = (mact m a, mact m b, mact m c, mact m d, mact m e)
#endif

instance (MSet m a, MSet m b, MSet m c, MSet m d, MSet m e, MSet m f) => MSet m (a, b, c, d, e, f) where
#if __GLASGOW_HASKELL__ <= 822
    mact m (a, b, c, d, e, f) = (mact m a, mact m b, mact m c, mact m d, mact m e, mact m f)
#endif

instance (MSet m a, MSet m b, MSet m c, MSet m d, MSet m e, MSet m f, MSet m h) => MSet m (a, b, c, d, e, f, h) where
#if __GLASGOW_HASKELL__ <= 822
    mact m (a, b, c, d, e, f, h) = (mact m a, mact m b, mact m c, mact m d, mact m e, mact m f, mact m h)
#endif

instance (MSet m a, MSet m b, MSet m c, MSet m d, MSet m e, MSet m f, MSet m h, MSet m i) => MSet m (a, b, c, d, e, f, h, i) where
#if __GLASGOW_HASKELL__ <= 822
    mact m (a, b, c, d, e, f, h, i) = (mact m a, mact m b, mact m c, mact m d, mact m e, mact m f, mact m h, mact m i)
#endif

instance MSet m a => MSet m [a] where
#if __GLASGOW_HASKELL__ <= 822
    mact m = map (mact m)
#endif

instance MSet m a => MSet m (NonEmpty a) where
#if __GLASGOW_HASKELL__ <= 822
    mact m = NE.map (mact m)
#endif

instance (MSet m a, Ord a) => MSet m (Set a) where
#if __GLASGOW_HASKELL__ <= 822
    mact m as = Set.map (mact m) as
#endif

{--
  - instance {-# OVERLAPPABLE #-} (Functor f, MSet m a) => MSet m (f a) where
  -     act m fa = fmap (act m) fa
  --}


#if __GLASGOW_HASKELL__ <= 822
fmact :: (Functor f, MSet s a) => s -> f a -> f a
fmact s = fmap (mact s)
#endif


instance MSet m a => MSet m (Identity a) where
#if __GLASGOW_HASKELL__ <= 822
    mact = fmact
#endif

instance MSet m a => MSet (Identity m) a where
#if __GLASGOW_HASKELL__ <= 822
    mact (Identity f) a = f `mact` a
#endif

instance MSet m a => MSet m (Maybe a) where
#if __GLASGOW_HASKELL__ <= 822
    mact = fmact
#endif

instance MSet m b => MSet m (Either a b) where
#if __GLASGOW_HASKELL__ <= 822
    mact = fmact
#endif

instance MSet m a => MSet m (Down a) where
#if __GLASGOW_HASKELL__ <= 822
    mact m (Down a) =  Down (mact m a)
#endif

instance MSet m a => MSet m (IO a) where
#if __GLASGOW_HASKELL__ <= 822
    mact = fmact
#endif

instance MSet m b => MSet m (a -> b) where
#if __GLASGOW_HASKELL__ <= 822
    mact = fmact
#endif

instance MSet (Endo a) a where
#if __GLASGOW_HASKELL__ <= 822
    mact = appEndo
#endif

instance {-# OVERLAPPABLE #-} MSet m a => MSet (S m) a where
#if __GLASGOW_HASKELL__ <= 822
    S m `mact` a = m `mact` a
#endif

instance {-# OVERLAPPING #-} MSet m b => MSet (S m) (Endo b) where
#if __GLASOW_HASKELL__ <= 822
    mact m (Endo f) = Endo $ mact m . f
#endif

instance Monoid m => MSet (Sum Natural) m where
#if __GLASOW_HASKELL__ <= 822
    mact (Sum 0) _ = mempty
    mact (Sum n) s = s `mappend` mact (Sum (n - 1)) s
#endif

instance MSet m a => MSet m (Const a b) where
#if __GLASOW_HASKELL__ <= 822
    mact s (Const a) = Const $ s `mact` a
#endif

instance (Functor f, Functor h, MSet m a) => MSet m (Functor.Product f h a) where
#if __GLASOW_HASKELL__ <= 822
    mact = fmact 
#endif

instance (Functor f, Functor h, MSet m a) => MSet m (Functor.Sum f h a) where
#if __GLASOW_HASKELL__ <= 822
    mact = fmact 
#endif

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

instance ( Monoid m
         ) => Monad (FreeMSet m) where
    return = returnFree
    (>>=)  = bindFree

instance Semigroup m => SSet m (FreeMSet m a) where
    act m (FreeMSet (h, a)) = FreeMSet (m <> h, a)

instance Monoid m => MSet m (FreeMSet m a) where
#if __GLASOW_HASKELL__ <= 822
    mact m (FreeMSet (h, a)) = FreeMSet (m `mappend` h, a)
#endif

-- |
-- @'foldrFree'@ for @'FreeMSet'@
foldrMSet :: forall m a b . MSet m b => (a -> b -> b) -> b -> (m, a) -> b
foldrMSet f b (m, a) = foldrFree f b (FreeMSet (S m, a))

type instance AlgebraType0 (FreeMSet m) a = ()
type instance AlgebraType  (FreeMSet m) a = MSet m a
instance ( Monoid m
         ) => FreeAlgebra (FreeMSet m) where
    returnFree a = FreeMSet (mempty, a)
    foldMapFree f (FreeMSet (m, a)) = mact m (f a)
    codom  = proof
    forget = proof
