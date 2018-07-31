module Data.Semigroup.Abelian
    ( AbelianSemigroup
    , FreeAbelianSemigroup
    , toNonEmpty
    , fromNonEmpty
    ) where

import           Data.IntSet (IntSet)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import           Data.Semigroup (Semigroup, All, Any, Dual, Max, Min, Option, Product, Sum)
import           Data.Void (Void)

import           Data.Algebra.Free (AlgebraType, FreeAlgebra (..))

-- |
-- Class of commutative monoids, e.g. with additional law:
-- @
--  a <> b = b <> a
-- @
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

instance Ord a => AbelianSemigroup (Set a)

instance AbelianSemigroup IntSet

-- |
-- Free abelian semigroup is isomorphic to a non empty map with keys @a@ and
-- values positive natural numbers.
newtype FreeAbelianSemigroup a = FreeAbelianSemigroup { runFreeAbelianSemigroup :: Map a Integer }
    deriving (Ord, Eq, Show)

toNonEmpty :: FreeAbelianSemigroup a -> NonEmpty (a, Integer)
toNonEmpty (FreeAbelianSemigroup as) = NE.fromList . Map.toList $ as

-- |
-- Smart constructor which creates `FreeAbelianSemigroup` from a non empty list
-- of pairs @(a, n) :: (a, Integer)@ where @n > 0@.
fromNonEmpty :: Ord a => NonEmpty (a, Integer) -> Maybe (FreeAbelianSemigroup a)
fromNonEmpty = fmap (FreeAbelianSemigroup . Map.fromList) . go . NE.toList
    where
    go [] = Just []
    go ((a, n) : as) | n < 0     = Nothing
                     | otherwise = ((a, n) :) <$> go as

instance Ord a => Semigroup (FreeAbelianSemigroup a) where
    (FreeAbelianSemigroup a) <> (FreeAbelianSemigroup b) = FreeAbelianSemigroup $ Map.unionWith (+) a b

type instance AlgebraType FreeAbelianSemigroup a = AbelianSemigroup a
instance FreeAlgebra FreeAbelianSemigroup where
    returnFree a = FreeAbelianSemigroup $ Map.singleton a 1
    foldMapFree f (FreeAbelianSemigroup as) = foldMapFree f (toNonEmpty_ as)
        where
        replicate_ :: a -> Integer -> [a]                                     
        replicate_ _ n | n <= 0 = error "foldMapFree @FreeAbelianSemigroup: impossible"
        replicate_ a 1 = [a]                                                   
        replicate_ a n = a : replicate_ a (n - 1)                             

        toNonEmpty_ :: Map a Integer -> NonEmpty a
        toNonEmpty_ = NE.fromList . concat . map (uncurry replicate_) . Map.toList
