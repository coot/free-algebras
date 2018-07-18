{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}
module Test.Control.Algebra.Free
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

import           Data.Algebra.Free ( AlgebraType )
import           Control.Algebra.Free
    ( AlgebraType1
    , FreeAlgebra1 (..)
    , fmapFree1
    , foldFree1
    , hoistFree1
    , iterFree1
    )

genIntToInt :: Integral n => Gen (n -> n)
genIntToInt = do
    x <- Gen.integral $ Range.linear (-100) 100
    return (+x)

showIntToInt :: (Integral n, Show n) => (n -> n) -> String
showIntToInt f = "(+"++ show (f 0) ++ ")"

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
      -> Gen (x -> x)
      -> Gen (Ap Maybe x)
genAp gen genf = Gen.sized $ \s -> go s
    where
    go (Range.Size 0) = Gen.maybe gen >>= \case
        Just x  -> return $ Ap.Pure x
        Nothing -> return $ Ap.Ap Nothing (Ap.Pure id)
    go s = do
        ap <- go (s - 1)
        f <- genf
        return $ Ap.Pure f <*> ap

genApIdentity
    :: forall f x . Show x
    => Gen x
    -> Gen (x -> x)
    -> Gen (Ap Identity x)
genApIdentity gen genf = Gen.sized $ \s -> go s
    where
    go (Range.Size 0) = do
        x <- gen
        return $ Ap.Ap (Identity x) (Ap.Pure id)
    go s = do
        ap <- go (s - 1)
        f <- genf
        return $ Ap.Pure f <*> ap


-- |
-- Generate  @Free Maybe@ of arbitrary depth.
genFree :: Gen x
        -> Gen (Free Maybe x)
genFree gen = Gen.sized go
    where
    go (Range.Size 0) = Free.Pure <$> gen
    go s = Free.Free <$> Gen.maybe (go (s - 1))

genFreeIdentity
    :: Gen x
    -> Gen (Free Identity x)
genFreeIdentity gen = Gen.sized go
    where
    go (Range.Size 0) = Free.Pure <$> gen
    go s = Free.Free . Identity <$> go (s - 1)

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
    -> Gen (a -> b)
    -> Property
fmapFree1_property gen show_mfa eq_mfa genf = property $ do
    mfa <- H.forAllWith show_mfa gen
    f <- H.forAllWith (\_ -> "(a -> b)") genf
    H.assert (fmapFree1 f mfa `eq_mfa` fmap f mfa)

prop_fmapFree1_coyoneda_maybe :: Property
prop_fmapFree1_coyoneda_maybe =
    fmapFree1_property
        (genCoyoneda toOdd)
        show
        (==)
        genIntToInt

prop_fmapFree1_ap :: Property
prop_fmapFree1_ap =
    fmapFree1_property
        (genAp (Gen.word8 (Range.linear 0 254)) genIntToInt)
        (show . Ap.retractAp)
        (\a b -> Ap.retractAp a == Ap.retractAp b)
        genIntToInt

prop_fmapFree1_free :: Property
prop_fmapFree1_free =
    fmapFree1_property
        (genFree $ Gen.word8 (Range.linear 0 254))
        show
        (==)
        genIntToInt

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
    (H.forAllWith (show . Ap.retractAp) $ genAp (Gen.integral $ Range.linear 0 100) genIntToInt)
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
    (genAp (Gen.int $ Range.linear 0 1000) genIntToInt)
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

iterFree1_property
    :: forall m f a
    .  ( FreeAlgebra1 m
       , AlgebraType m f
       , AlgebraType1 m f
       , AlgebraType m Identity
       , AlgebraType1 m Identity
       , Eq a
       , Show a
       )
    => Gen (m f a)
    -> (m f a -> String)
    -> (forall x. f x -> x)
    -> ((forall x . f x -> x) -> m f a -> a)
    -- ^ reference implementation
    -> Property
iterFree1_property gen show_mfa nat refImpl = property $ do
    mfa <- H.forAllWith show_mfa gen
    iterFree1 nat mfa === refImpl nat mfa

prop_iterFree1_coyoneda :: Property
prop_iterFree1_coyoneda = iterFree1_property
    (genCoyoneda Identity)
    show
    runIdentity
    (\_ -> runIdentity . lowerCoyoneda)

prop_iterFree1_free :: Property
prop_iterFree1_free = iterFree1_property
    (genFreeIdentity (Gen.int $ Range.linear 0 1000))
    show
    runIdentity
    Free.iter

prop_iterFree1_ap :: Property
prop_iterFree1_ap = iterFree1_property
    (genApIdentity (Gen.int $ Range.linear 0 1000) genIntToInt)
    (show . Ap.retractAp)
    runIdentity
    Ap.iterAp
    where

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
