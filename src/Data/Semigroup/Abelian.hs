module Data.Semigroup.Abelian
    ( AbelianSemigroup
    ) where

import           Data.Semigroup (Semigroup, All, Any, Dual, Max, Min, Option, Product, Sum)

-- |
-- Class of commutative monoids
class Semigroup m => AbelianSemigroup m

instance AbelianSemigroup All
instance AbelianSemigroup Any
instance AbelianSemigroup a => AbelianSemigroup (Dual a)
instance Ord a => AbelianSemigroup (Max a)
instance Ord a => AbelianSemigroup (Min a)
instance AbelianSemigroup a => AbelianSemigroup (Option a)
instance Num a => AbelianSemigroup (Product a)
instance Num a => AbelianSemigroup (Sum a)
instance AbelianSemigroup a => AbelianSemigroup (IO a)
