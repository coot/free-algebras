{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Algebra.Pointed
    ( Pointed (..)
    , PointedMonoid (..)
    ) where

#if __GLASGOW_HASKELL__ <= 802
import Data.Semigroup (Semigroup (..))
#endif

-- | Class of pointed sets
--
class Pointed p where
    point :: p

instance Pointed (Maybe a) where
    point = Nothing

-- | @Monoid@ should be a subclass of @Pointed@.
--
newtype PointedMonoid m = PointedMonoid { runPointedMonoid :: m }
    deriving (Show, Eq, Ord, Functor)

instance Semigroup m => Semigroup (PointedMonoid m) where
    (PointedMonoid m) <> (PointedMonoid n) = PointedMonoid (m <> n)

instance Monoid m => Monoid (PointedMonoid m) where
    mempty = PointedMonoid mempty
#if __GLASGOW_HASKELL__ <= 802
    mappend (PointedMonoid m) (PointedMonoid n) = PointedMonoid $ mappend m n
#endif

instance Monoid m => Pointed (PointedMonoid m) where
    point = mempty
