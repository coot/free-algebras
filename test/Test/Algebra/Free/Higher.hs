{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}
module Test.Algebra.Free.Higher
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
import           Algebra.Free.Higher
    ( AlgebraTypeH
    , FreeAlgebraH (..)
    , fmapH
    , foldH
    , hoistH
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

fmapH_property
    :: forall m f a b
    .  ( FreeAlgebraH m
       , AlgebraTypeH m f
       , AlgebraType m f
       , AlgebraType m (m f)
       , Functor (m f)
       )
    => Gen (m f a)
    -> (m f a -> String)
    -> (m f b -> m f b -> Bool)
    -> (a -> b)
    -> Property
fmapH_property gen show_mfa eq_mfa f = property $ do
    mfa <- H.forAllWith show_mfa gen
    H.assert (fmapH f mfa `eq_mfa` fmap f mfa)

prop_fmapH_coyoneda_maybe :: Property
prop_fmapH_coyoneda_maybe =
    fmapH_property
        (genCoyoneda toOdd)
        show
        (==)
        (\x -> 2*x + 1)

prop_fmapH_ap :: Property
prop_fmapH_ap =
    fmapH_property
        (genAp (Gen.word8 (Range.linear 0 254)) (\x -> 2 * x + 1))
        (show . Ap.retractAp)
        (\a b -> Ap.retractAp a == Ap.retractAp b)
        (+1)

prop_fmapH_free :: Property
prop_fmapH_free =
    fmapH_property
        (genFree $ Gen.word8 (Range.linear 0 254))
        show
        (==)
        (\x -> 2*x + 1)

foldH_property
    :: forall m f a
    .  ( FreeAlgebraH m
       , AlgebraType m f
       , AlgebraTypeH m f
       , Eq (f a)
       , Show (f a)
       )
    => PropertyT IO (m f a)
    -> (m f a -> f a)
    -- ^ reference fold implentation
    -> Property
foldH_property gen fold_ = property $ do
    mfa <- gen
    foldH mfa === fold_ mfa

prop_foldH_coyoneda :: Property
prop_foldH_coyoneda =
    foldH_property (H.forAll $ genCoyoneda toOdd) lowerCoyoneda

prop_foldH_ap :: Property
prop_foldH_ap = foldH_property
    (H.forAllWith (show . Ap.retractAp) $ genAp (Gen.integral $ Range.linear 0 100) (+1))
    Ap.retractAp

prop_foldH_free :: Property
prop_foldH_free = foldH_property
    (H.forAll $ genFree (Gen.integral $ Range.linear 0 100))
    (Free.foldFree id)

hoistH_property
    :: forall m f g a
    .  ( FreeAlgebraH m
       , AlgebraType m f
       , AlgebraType m (m g)
       , AlgebraTypeH m f
       , AlgebraType m g
       , AlgebraTypeH m g
       )
    => Gen (m f a)
    -> (m f a -> String)
    -> (m g a -> m g a -> Bool)
    -> (forall x. f x -> g x)
    -> ((forall x . f x -> g x) -> m f a -> m g a)
    -- ^ reference hoist impelentation
    -> Property
hoistH_property gen show_mfa eq_mga nat refImpl = property $ do
    mfa <- H.forAllWith show_mfa gen
    H.assert $ hoistH nat mfa `eq_mga` refImpl nat mfa

prop_hoistH_coyoneda :: Property
prop_hoistH_coyoneda = hoistH_property
    (genCoyoneda toOdd)
    (show . lowerCoyoneda)
    (\a b -> lowerCoyoneda a == lowerCoyoneda b)
    (maybe (Left ()) Right)
    (\nat (Coyoneda xa fx) -> Coyoneda xa (nat fx))

prop_hoistH_ap :: Property
prop_hoistH_ap = hoistH_property
    (genAp (Gen.int $ Range.linear 0 1000) (+1))
    (show . Ap.retractAp)
    (\x y -> Ap.retractAp x == Ap.retractAp y)
    (maybe (Left ()) Right)
    Ap.hoistAp

prop_hoistH_free :: Property
prop_hoistH_free = hoistH_property
    (genFree (Gen.integral $ Range.linear 0 100))
    show
    (==)
    (maybe (Left ()) Right)
    Free.hoistFree

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
