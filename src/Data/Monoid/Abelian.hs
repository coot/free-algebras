module Data.Monoid.Abelian
    ( FreeAbelianMonoid (..)
    ) where

import           Data.Constraint (Dict (..))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (stimes)
import           Data.Natural (Natural)

import           Data.Algebra.Free (AlgebraType, AlgebraType0, FreeAlgebra (..), Proof (..))
import           Data.Semigroup.Abelian (AbelianSemigroup)

-- |
-- Free abelian monoid.  Note that `FreeAbelianMonoid () ≅ Natural` as
-- expected.
newtype FreeAbelianMonoid a = FreeAbelianMonoid (Map a Natural)
    deriving (Eq, Ord, Show)

instance Ord a => Semigroup (FreeAbelianMonoid a) where
    (FreeAbelianMonoid a) <> (FreeAbelianMonoid b) = FreeAbelianMonoid $ Map.unionWith (+) a b

instance Ord a => AbelianSemigroup (FreeAbelianMonoid a)

instance Ord a => Monoid (FreeAbelianMonoid a) where
    mempty = FreeAbelianMonoid (Map.empty)

type instance AlgebraType0 FreeAbelianMonoid a = Ord a
type instance AlgebraType  FreeAbelianMonoid m = (Monoid m, AbelianSemigroup m)
instance FreeAlgebra FreeAbelianMonoid where
    returnFree a = FreeAbelianMonoid (Map.singleton a 1)
    foldMapFree g (FreeAbelianMonoid as) = Map.foldMapWithKey (\a n -> stimes n $ g a) as 
    proof0 = Proof Dict
    proof  = Proof Dict
