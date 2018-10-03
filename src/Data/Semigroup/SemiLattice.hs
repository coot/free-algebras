{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Semigroup.SemiLattice
    ( FreeSemiLattice
    , fromNonEmpty
    , toNonEmpty
    ) where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.IntSet (IntSet)
import           Data.Semigroup (All, Any, Semigroup, sconcat)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)

import           Data.Algebra.Free
    ( AlgebraType
    , AlgebraType0
    , FreeAlgebra (..)
    , proof
    )
import           Data.Semigroup.Abelian (AbelianSemigroup)

-- |
-- Class of abelian semigroups in which every element is idempontent, i.e.
-- @a <> a = a@.
class AbelianSemigroup m => SemiLattice m

instance SemiLattice Void
instance SemiLattice ()
instance SemiLattice All
instance SemiLattice Any
instance Ord a => SemiLattice (Set a)
instance SemiLattice IntSet

-- |
-- @'FreeSemiLattice'@ is a non empty set.
newtype FreeSemiLattice a = FreeSemiLattice (Set a)
    deriving (Ord, Eq, Show, Semigroup)

instance Ord a => AbelianSemigroup (FreeSemiLattice a)

instance Ord a => SemiLattice (FreeSemiLattice a)

fromNonEmpty :: Ord a => NonEmpty a -> FreeSemiLattice a
fromNonEmpty = FreeSemiLattice . Set.fromList . NE.toList

toNonEmpty :: FreeSemiLattice a -> NonEmpty a
toNonEmpty (FreeSemiLattice as) = NE.fromList $ Set.toList as

type instance AlgebraType0 FreeSemiLattice a = Ord a
type instance AlgebraType  FreeSemiLattice a = (Ord a, SemiLattice a)
instance FreeAlgebra FreeSemiLattice where
    returnFree a = FreeSemiLattice $ Set.singleton a
    foldMapFree f (FreeSemiLattice as) = sconcat $ fmap f $ NE.fromList $ Set.toList as

    codom  = proof
    forget = proof
