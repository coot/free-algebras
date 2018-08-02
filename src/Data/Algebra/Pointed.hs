{-# LANGUAGE UndecidableInstances #-}
module Data.Algebra.Pointed
    ( Pointed (..)
    , PointedMonoid (..)
    ) where


-- |
-- Class of pointed sets
class Pointed p where
    point :: p

instance Pointed (Maybe a) where
    point = Nothing

-- |
-- @Monoid@ should be a subclass of @Pointed@.
newtype PointedMonoid m = PointedMonoid { runPointedMonoid :: m }
    deriving (Show, Eq, Ord, Functor)

instance Semigroup m => Semigroup (PointedMonoid m) where
    (PointedMonoid m) <> (PointedMonoid n) = PointedMonoid (m <> n)

instance Monoid m => Monoid (PointedMonoid m) where
    mempty = PointedMonoid mempty

instance Monoid m => Pointed (PointedMonoid m) where
    point = mempty
