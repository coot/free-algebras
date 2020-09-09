{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Monoid.Abelian
    ( FreeAbelianMonoid (..)
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup (Semigroup (..), stimes)
import           Numeric.Natural (Natural)

import           Data.Algebra.Free (AlgebraType, AlgebraType0, FreeAlgebra (..))
import           Data.Semigroup.Abelian (AbelianSemigroup)

-- | Free abelian monoid.  Note that `FreeAbelianMonoid () ≅ Natural` as
-- expected.
--
-- It is a monad on the full subcategory which satisfies the `Ord` constraint,
-- but base does not allow to define a functor \/ applicative \/ monad
-- instances which are constraint by a class.
--
newtype FreeAbelianMonoid a = FreeAbelianMonoid {
        runFreeAbelianMonoid :: Map a Natural
    }
    deriving (Eq, Ord, Show)

instance Ord a => Semigroup (FreeAbelianMonoid a) where
    FreeAbelianMonoid a <> FreeAbelianMonoid b =
        FreeAbelianMonoid $ Map.unionWith (+) a b

instance Ord a => AbelianSemigroup (FreeAbelianMonoid a)

instance Ord a => Monoid (FreeAbelianMonoid a) where
    mempty = FreeAbelianMonoid Map.empty
#if __GLASGOW_HASKELL__ <= 802
    mappend = (<>)
#endif

type instance AlgebraType0 FreeAbelianMonoid a = Ord a
type instance AlgebraType  FreeAbelianMonoid m = (Ord m, Monoid m, AbelianSemigroup m)

instance FreeAlgebra FreeAbelianMonoid where
    returnFree a = FreeAbelianMonoid (Map.singleton a 1)

    foldMapFree g (FreeAbelianMonoid as)
                 = Map.foldMapWithKey (\a n -> stimes n $ g a) as
