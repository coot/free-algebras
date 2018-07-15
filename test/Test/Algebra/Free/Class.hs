{-# LANGUAGE TemplateHaskell #-}
module Test.Algebra.Free.Class
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

import           Algebra.Free.Class
    ( AlgebraType
    , FreeAlgebra (..)
    , foldF
    , natF
    , fmapF
    , joinF
    , bindF
    )

natF_property
    :: ( FreeAlgebra f
       , AlgebraType f (f a)
       , Eq (f a)
       , Show (f a)
       )
    => Gen (f a) -> Property
natF_property gen = property $ do
    fa <- H.forAll gen
    natF fa === fa

prop_natF_list :: Property
prop_natF_list = natF_property
    $ Gen.list (Range.linear 0 100) Gen.alpha

prop_nafF_nonempty :: Property
prop_nafF_nonempty = natF_property
    $ Gen.nonEmpty (Range.linear 0 100) Gen.alpha

-- |
-- Check that @'foldF' is @'fold'@ for @f@ which are @'Foldable'@ and @a@ which
-- are @'Monoid' a.
foldF_property
    :: ( FreeAlgebra f
       , AlgebraType f (f a)
       , AlgebraType f a
       , Monoid a   -- fold brings this constraint, @'foldF'@ is free of it!
       , Foldable f
       , Eq a
       , Eq (f a)
       , Show a
       , Show (f a)
       )
    => Gen (f a)
    -> Property
foldF_property gen = property $ do
    fa <- H.forAll gen
    foldF fa === fold fa

prop_foldF_list :: Property
prop_foldF_list = foldF_property 
    $ (Gen.list $ Range.linear 0 100)
        (Sum <$> Gen.word32 (Range.linear 0 1024))

prop_foldF_nonempty :: Property
prop_foldF_nonempty = foldF_property
    $ (Gen.nonEmpty $ Range.linear 0 100)
        (Sum <$> Gen.word32 (Range.linear 0 1024))

-- |
-- @'fmapF'@ should aggree with @'fmap'@ for types which satisfy @'Functor'@
-- constraint.
fmapF_property
    :: ( FreeAlgebra f
       , AlgebraType f (f b)
       , Functor f
       , Show (f a)
       , Eq (f a)
       , Show (f b)
       , Eq (f b)
       )
    => Gen (f a)
    -> (a -> b)
    -> Property
fmapF_property gen f = property $ do
    fa <- H.forAll gen
    fmapF f fa === fmap f fa

prop_fmapF_list :: Property
prop_fmapF_list = fmapF_property
    ((Gen.list $ Range.linear 0 100)
        (Gen.integral $ Range.linear 0 1024))
    (\x -> x^2 + 2 * x + 1)

prop_fmapF_nonempty :: Property
prop_fmapF_nonempty = fmapF_property
    ((Gen.nonEmpty $ Range.linear 0 100)
        (Gen.integral $ Range.linear 0 1024))
    (\x -> 2 * x + 1)

-- |
-- @'joinF'@ should be equal to @'join'@ for monads.
joinF_property
    :: ( FreeAlgebra m
       , AlgebraType m (m a)
       , Monad m
       , Show (m (m a))
       , Eq (m (m a))
       , Show (m a)
       , Eq (m a)
       )
    => Gen (m (m a))
    -> Property
joinF_property gen = property $ do
    mma <- H.forAll gen
    joinF mma === join mma

prop_joinF_list :: Property
prop_joinF_list =
    let gen = Gen.list (Range.linear 0 100)
                (Gen.list (Range.linear 0 10) Gen.alpha)
    in joinF_property gen

prop_joinF_nonempty :: Property
prop_joinF_nonempty =
    let gen = Gen.nonEmpty (Range.linear 0 100)
                (Gen.nonEmpty (Range.linear 0 10) Gen.alpha)
    in joinF_property gen

-- |
-- @'bindF'@ should be equal to @'>>='@ for monads.
bindF_property
    :: ( FreeAlgebra m
       , AlgebraType m (m a)
       , AlgebraType m (m b)
       , AlgebraType m (m (m b))
       , Monad m
       , Show (m a)
       , Eq (m a)
       , Show (m b)
       , Eq (m b)
       )
    => Gen (m a)
    -> (a -> m b)
    -> Property
bindF_property gen f = property $ do
    ma <- H.forAll gen
    bindF ma f === (ma >>= f)

prop_bindF_list :: Property
prop_bindF_list =
    let gen = Gen.list
            (Range.linear 0 10)
            (Gen.integral $ Range.linear 0 1024)
    in bindF_property gen (\x -> [x^2, 2 * x, 1])

prop_bindF_nonempty :: Property
prop_bindF_nonempty =
    let gen = Gen.nonEmpty
            (Range.linear 0 10)
            (Gen.integral $ Range.linear 0 1024)
    in bindF_property gen (\x -> x^2 :| [2 * x, 1])

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
