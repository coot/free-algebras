{-# LANGUAGE UndecidableInstances #-}
module Data.Semigroup.Abelian
    ( AbelianSemigroup
    ) where

import           Data.IntSet (IntSet)
import           Data.Set (Set)
import           Data.Semigroup (Semigroup, All, Any, Dual, Max, Min, Option, Product, Sum)
import           Data.Void (Void)

-- |
-- Class of commutative monoids
class Semigroup m => AbelianSemigroup m

instance AbelianSemigroup Void
instance AbelianSemigroup ()
instance AbelianSemigroup All
instance AbelianSemigroup Any
instance AbelianSemigroup a => AbelianSemigroup (Dual a)
instance Ord a => AbelianSemigroup (Max a)
instance Ord a => AbelianSemigroup (Min a)
instance AbelianSemigroup a => AbelianSemigroup (Option a)
instance Num a => AbelianSemigroup (Product a)
instance Num a => AbelianSemigroup (Sum a)
instance AbelianSemigroup a => AbelianSemigroup (IO a)
instance Ord a => AbelianSemigroup (Set a)
instance AbelianSemigroup IntSet
