{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}
module Test.Algebra.Free.Class1
    ( tests
    ) where

import           Control.Applicative.Free (Ap)
import qualified Control.Applicative.Free as Ap
import           Control.Monad.Free (Free)
import qualified Control.Monad.Free as Free
import           Control.Monad (join)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Foldable (fold)
import           Data.Functor.Identity (Identity (..))
import           Data.Functor.Coyoneda (Coyoneda (..), lowerCoyoneda)
import           Data.Monoid (Sum (..))
import           Hedgehog (Property, PropertyT, Gen, property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Algebra.Free.Class ( AlgebraType )
import           Algebra.Free.Class1
    ( AlgebraType1
    , FreeAlgebra1 (..)
    , fmapFree1
    , foldFree1
    , hoistFree1
    )

-- |
-- Generate a @Coyoneda f@ given a constructor of @f@.
genCoyoneda
    :: (Int -> f Int)
    -> Gen (Coyoneda f Int)
genCoyoneda f = do
    a <- Gen.int $ Range.linear 0 100
    Gen.bool_ >>= \case
        True  -> return $ Coyoneda id (f a)
        False -> do
            x <- Gen.int $ Range.linear 0 100 
            return $ Coyoneda (\x -> x + a) (f x)

toOdd :: Integral n => n -> Maybe n
toOdd x = if x `mod` 2 == 0
            then Nothing
            else Just x

-- |
-- Generated `Ap Maybe` with arbitrary depth.
genAp :: forall f x . Show x
      => Gen x
      -> (x -> x)
      -> Gen (Ap Maybe x)
genAp gen f = Gen.sized $ \s -> go s
    where
    go (Range.Size 0) = Gen.maybe gen >>= \case
        Just x  -> return $ Ap.Pure x
        Nothing -> return $ Ap.Ap Nothing (Ap.Pure id)
    go s = do
        ap <- go (s - 1)
        return $ Ap.Pure f <*> ap

-- |
-- Generate  @Free Maybe@ of arbitrary depth.
genFree :: Gen x
        -> Gen (Free Maybe x)
genFree gen = Gen.sized go
    where
    go (Range.Size 0) = Free.Pure <$> gen
    go s = Free.Free <$> Gen.maybe (go (s - 1))

fmapFree1_property
    :: forall m f a b
    .  ( FreeAlgebra1 m
       , AlgebraType1 m f
       , AlgebraType m f
       , AlgebraType m (m f)
       , Functor (m f)
       )
    => Gen (m f a)
    -> (m f a -> String)
    -> (m f b -> m f b -> Bool)
    -> (a -> b)
    -> Property
fmapFree1_property gen show_mfa eq_mfa f = property $ do
    mfa <- H.forAllWith show_mfa gen
    H.assert (fmapFree1 f mfa `eq_mfa` fmap f mfa)

prop_fmapFree1_coyoneda_maybe :: Property
prop_fmapFree1_coyoneda_maybe =
    fmapFree1_property
        (genCoyoneda toOdd)
        show
        (==)
        (\x -> 2*x + 1)

prop_fmapFree1_ap :: Property
prop_fmapFree1_ap =
    fmapFree1_property
        (genAp (Gen.word8 (Range.linear 0 254)) (\x -> 2 * x + 1))
        (show . Ap.retractAp)
        (\a b -> Ap.retractAp a == Ap.retractAp b)
        (+1)

prop_fmapFree1_free :: Property
prop_fmapFree1_free =
    fmapFree1_property
        (genFree $ Gen.word8 (Range.linear 0 254))
        show
        (==)
        (\x -> 2*x + 1)

foldFree1_property
    :: forall m f a
    .  ( FreeAlgebra1 m
       , AlgebraType m f
       , AlgebraType1 m f
       , Eq (f a)
       , Show (f a)
       )
    => PropertyT IO (m f a)
    -> (m f a -> f a)
    -- ^ reference fold implentation
    -> Property
foldFree1_property gen fold_ = property $ do
    mfa <- gen
    foldFree1 mfa === fold_ mfa

prop_foldFree1_coyoneda :: Property
prop_foldFree1_coyoneda =
    foldFree1_property (H.forAll $ genCoyoneda toOdd) lowerCoyoneda

prop_foldFree1_ap :: Property
prop_foldFree1_ap = foldFree1_property
    (H.forAllWith (show . Ap.retractAp) $ genAp (Gen.integral $ Range.linear 0 100) (+1))
    Ap.retractAp

prop_foldFree1_free :: Property
prop_foldFree1_free = foldFree1_property
    (H.forAll $ genFree (Gen.integral $ Range.linear 0 100))
    (Free.foldFree id)

hoistFree1_property
    :: forall m f g a
    .  ( FreeAlgebra1 m
       , AlgebraType m f
       , AlgebraType m (m g)
       , AlgebraType1 m f
       , AlgebraType m g
       , AlgebraType1 m g
       )
    => Gen (m f a)
    -> (m f a -> String)
    -> (m g a -> m g a -> Bool)
    -> (forall x. f x -> g x)
    -> ((forall x . f x -> g x) -> m f a -> m g a)
    -- ^ reference hoist impelentation
    -> Property
hoistFree1_property gen show_mfa eq_mga nat refImpl = property $ do
    mfa <- H.forAllWith show_mfa gen
    H.assert $ hoistFree1 nat mfa `eq_mga` refImpl nat mfa

prop_hoistFree1_coyoneda :: Property
prop_hoistFree1_coyoneda = hoistFree1_property
    (genCoyoneda toOdd)
    (show . lowerCoyoneda)
    (\a b -> lowerCoyoneda a == lowerCoyoneda b)
    (maybe (Left ()) Right)
    (\nat (Coyoneda xa fx) -> Coyoneda xa (nat fx))

prop_hoistFree1_ap :: Property
prop_hoistFree1_ap = hoistFree1_property
    (genAp (Gen.int $ Range.linear 0 1000) (+1))
    (show . Ap.retractAp)
    (\x y -> Ap.retractAp x == Ap.retractAp y)
    (maybe (Left ()) Right)
    Ap.hoistAp

prop_hoistFree1_free :: Property
prop_hoistFree1_free = hoistFree1_property
    (genFree (Gen.integral $ Range.linear 0 100))
    show
    (==)
    (maybe (Left ()) Right)
    Free.hoistFree

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
