module Data.Monoid.Abelian.Free
    ( FreeAbelianMonoid (..)
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (stimes)
import           Data.Natural (Natural)

import           Data.Algebra.Free (AlgebraType, FreeAlgebra (..))
import           Data.Semigroup.Abelian (AbelianSemigroup)

-- |
-- Free abelian monoid.  Note that `FreeAbelianMonoid () ≅ Natural` as
-- expected.
newtype FreeAbelianMonoid a = FreeAbelianMonoid (Map a Natural)

instance Ord a => Semigroup (FreeAbelianMonoid a) where
    (FreeAbelianMonoid a) <> (FreeAbelianMonoid b) = FreeAbelianMonoid $ Map.unionWith (+) a b

instance Ord a => AbelianSemigroup (FreeAbelianMonoid a)

instance Ord a => Monoid (FreeAbelianMonoid a) where
    mempty = FreeAbelianMonoid (Map.empty)

type instance AlgebraType FreeAbelianMonoid m = (Monoid m, AbelianSemigroup m)
instance FreeAlgebra FreeAbelianMonoid where
    returnFree a = FreeAbelianMonoid (Map.singleton a 1)
    foldMapFree g (FreeAbelianMonoid as) = Map.foldMapWithKey (\a n -> stimes n $ g a) as 
