{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Data.Semigroup.Semilattice
    ( FreeSemilattice
    , fromNonEmpty
    , toNonEmpty
    ) where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.IntSet (IntSet)
import           Data.Semigroup ( All
                                , Any
                                , sconcat)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)

import           Data.Algebra.Free ( AlgebraType
                                   , AlgebraType0
                                   , FreeAlgebra (..)
                                   )
import           Data.Semigroup.Abelian (AbelianSemigroup)

-- | Class of abelian semigroups in which every element is idempontent, i.e.
-- @a <> a = a@.
--
class AbelianSemigroup m => Semilattice m

instance Semilattice Void
instance Semilattice ()
instance Semilattice All
instance Semilattice Any
instance Ord a => Semilattice (Set a)
instance Semilattice IntSet

-- | @'FreeSemilattice'@ is a non empty set.
--
newtype FreeSemilattice a = FreeSemilattice (Set a)
    deriving (Ord, Eq, Show, Semigroup)

instance Ord a => AbelianSemigroup (FreeSemilattice a)

instance Ord a => Semilattice (FreeSemilattice a)

fromNonEmpty :: Ord a => NonEmpty a -> FreeSemilattice a
fromNonEmpty = FreeSemilattice . Set.fromList . NE.toList

toNonEmpty :: FreeSemilattice a -> NonEmpty a
toNonEmpty (FreeSemilattice as) = NE.fromList $ Set.toList as

type instance AlgebraType0 FreeSemilattice a = Ord a
type instance AlgebraType  FreeSemilattice a = (Ord a, Semilattice a)
instance FreeAlgebra FreeSemilattice where
    returnFree a = FreeSemilattice $ Set.singleton a
    foldMapFree f (FreeSemilattice as) = sconcat $ fmap f $ NE.fromList $ Set.toList as
