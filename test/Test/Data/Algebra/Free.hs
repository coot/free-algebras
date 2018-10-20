{-# LANGUAGE TemplateHaskell #-}
module Test.Data.Algebra.Free
    ( tests
    ) where

import           Control.Monad (join)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Foldable (fold)
import           Data.Monoid (Sum (..))
import           Hedgehog (Property, Gen, property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Data.Algebra.Free
    ( AlgebraType
    , AlgebraType0
    , FreeAlgebra (..)
    , foldFree
    , unFoldMapFree
    , natFree
    , fmapFree
    , joinFree
    , bindFree
    )

natFree_property
    :: ( FreeAlgebra  f
       , AlgebraType0 f a
       , AlgebraType  f (f a)
       , Eq (f a)
       , Show (f a)
       )
    => Gen (f a) -> Property
natFree_property gen = property $ do
    fa <- H.forAll gen
    natFree fa === fa

prop_natFree_list :: Property
prop_natFree_list = natFree_property
    $ Gen.list (Range.linear 0 100) Gen.alpha

prop_nafF_nonempty :: Property
prop_nafF_nonempty = natFree_property
    $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha

-- |
-- Check that @'foldFree' is @'fold'@ for @f@ which are @'Foldable'@ and @a@ which
-- are @'Monoid' a.
foldFree_property
    :: ( FreeAlgebra  f
       , AlgebraType0 f a
       , AlgebraType  f a
       , Monoid a   -- fold brings this constraint, @'foldFree'@ is free of it!
       , Foldable f
       , Eq a
       , Eq (f a)
       , Show a
       , Show (f a)
       )
    => Gen (f a)
    -> Property
foldFree_property gen = property $ do
    fa <- H.forAll gen
    foldFree fa === fold fa

prop_foldFree_list :: Property
prop_foldFree_list = foldFree_property 
    $ (Gen.list $ Range.linear 0 100)
        (Sum <$> Gen.word32 (Range.linear 0 1024))

prop_foldFree_nonempty :: Property
prop_foldFree_nonempty = foldFree_property
    $ (Gen.nonEmpty $ Range.linear 0 100)
        (Sum <$> Gen.word32 (Range.linear 0 1024))

-- |
-- @'fmapFoldFree'@ is inverse of @'unFoldMapFree'@
foldMapFree_property
    :: forall f d a .
       ( FreeAlgebra  f
       , AlgebraType0 f d
       , AlgebraType0 f a
       , AlgebraType  f d
       , Show (f a)
       , Show a
       , Show d
       , Eq d
       )
    => Gen (f a)
    -> Gen a
    -> (f a -> d)
    -> (a -> d)
    -> Property
foldMapFree_property gen_fa gen fad ad = property $ do
    fa <- H.forAll gen_fa
    a  <- H.forAll gen
    unFoldMapFree (foldMapFree @f ad) a === ad a
    foldMapFree (unFoldMapFree @f fad) fa === fad fa

prop_foldMapFree_list :: Property
prop_foldMapFree_list
    = foldMapFree_property @[] @(Sum Int) @Int
        ((Gen.list $ Range.linear 0 100)
            (Gen.integral $ Range.linear 0 1024))
        (Gen.integral @_ @Int $ Range.linear 0 1024)
        (Sum . sum)
        Sum

prop_foldMapFree_nonempty :: Property
prop_foldMapFree_nonempty
    = foldMapFree_property @NonEmpty @(Sum Int) @Int
        ((Gen.nonEmpty $ Range.linear 0 100)
            (Gen.integral @_ @Int $ Range.linear 0 1024))
        (Gen.integral @_ @Int $ Range.linear 0 1024)
        (Sum . sum)
        Sum

-- |
-- @'fmapFree'@ should aggree with @'fmap'@ for types which satisfy @'Functor'@
-- constraint.
fmapFree_property
    :: forall f a b .
       ( FreeAlgebra  f
       , AlgebraType0 f a
       , AlgebraType0 f b
       , Functor f
       , Show (f a)
       , Eq (f a)
       , Show (f b)
       , Eq (f b)
       )
    => Gen (f a)
    -> (a -> b)
    -> Property
fmapFree_property gen f = property $ do
    fa <- H.forAll gen
    fmapFree f fa === fmap f fa

prop_fmapFree_list :: Property
prop_fmapFree_list = fmapFree_property @[] @Integer @Integer
    ((Gen.list $ Range.linear 0 100)
        (Gen.integral $ Range.linear 0 1024))
    (\x -> x^(2::Int) + 2 * x + 1)

prop_fmapFree_nonempty :: Property
prop_fmapFree_nonempty = fmapFree_property
    ((Gen.nonEmpty $ Range.linear 0 100)
        (Gen.integral @_ @Int $ Range.linear 0 1024))
    (\x -> x^(2::Int) + 2 * x + 1)

-- |
-- @'joinFree'@ should be equal to @'join'@ for monads.
joinFree_property
    :: ( FreeAlgebra  m
       , AlgebraType0 m a
       , AlgebraType0 m (m a)
       , AlgebraType  m (m a)
       , Monad m
       , Show (m (m a))
       , Eq (m (m a))
       , Show (m a)
       , Eq (m a)
       )
    => Gen (m (m a))
    -> Property
joinFree_property gen = property $ do
    mma <- H.forAll gen
    joinFree mma === join mma

prop_joinFree_list :: Property
prop_joinFree_list =
    let gen = Gen.list (Range.linear 0 100)
                (Gen.list (Range.linear 0 10) Gen.alpha)
    in joinFree_property gen

prop_joinFree_nonempty :: Property
prop_joinFree_nonempty =
    let gen = Gen.nonEmpty (Range.linear 0 100)
                (Gen.nonEmpty (Range.linear 0 10) Gen.alpha)
    in joinFree_property gen

-- |
-- @'bindFree'@ should be equal to @'>>='@ for monads.
bindFree_property
    :: ( FreeAlgebra  m
       , AlgebraType0 m a
       , AlgebraType0 m b
       , AlgebraType0 m (m b)
       , AlgebraType  m (m a)
       , AlgebraType  m (m b)
       , AlgebraType  m (m (m b))
       , Monad m
       , Show (m a)
       , Eq (m a)
       , Show (m b)
       , Eq (m b)
       )
    => Gen (m a)
    -> (a -> m b)
    -> Property
bindFree_property gen f = property $ do
    ma <- H.forAll gen
    bindFree ma f === (ma >>= f)

prop_bindFree_list :: Property
prop_bindFree_list =
    let gen = Gen.list
            (Range.linear 0 10)
            (Gen.integral @_ @Int $ Range.linear 0 1024)
    in bindFree_property gen (\x -> [x^(2 :: Int), 2 * x, 1])

prop_bindFree_nonempty :: Property
prop_bindFree_nonempty =
    let gen = Gen.nonEmpty
            (Range.linear 0 10)
            (Gen.integral @_ @Int $ Range.linear 0 1024)
    in bindFree_property gen (\x -> x^(2 :: Int) :| [2 * x, 1])

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
