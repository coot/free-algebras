{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Control.Algebra.Free
    ( tests
    ) where

import           Control.Applicative.Free (Ap)
import qualified Control.Applicative.Free as Ap
import           Control.Monad.Free (Free)
import qualified Control.Monad.Free as Free
import qualified Control.Monad.State.Strict as S
import           Data.Functor (($>))
import           Data.Functor.Identity (Identity (..))
import           Data.Functor.Coyoneda (Coyoneda (..), lowerCoyoneda)
import           Hedgehog (Property, PropertyT, Gen, property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Text.Show.Functions ()

import           Data.Algebra.Free ( AlgebraType )
import           Data.Proxy (Proxy (..))
import           Control.Algebra.Free
                    ( AlgebraType0
                    , FreeAlgebra1 (..)
                    , Proof (..)
                    , unFoldNatFree
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
            return $ Coyoneda (\y -> y + a) (f x)

toOdd :: Integral n => n -> Maybe n
toOdd x = if x `mod` 2 == 0
            then Nothing
            else Just x

-- |
-- Generated `Ap Maybe` with arbitrary depth.
genAp :: forall x . Show x
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
    :: forall x . Show x
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

foldMapFree1_property
    :: forall m f d a
    .  ( FreeAlgebra1 m
       , AlgebraType  m d
       , AlgebraType  m f
       , AlgebraType0 m f
       , Show    a
       , Show (f a)
       , Eq   (f a)
       , Eq   (d a)
       , Show (d a)
       )
    => Gen (m f a)
    -> Gen (f a)
    -> (forall x. f x -> d x)
    -> (forall x. m f x -> d x)
    -> Property
foldMapFree1_property gen_mfa gen_fa fd mfd
    = property $ do
        mfa <- H.forAllWith (show . foldFree1) gen_mfa
        fa  <- H.forAll gen_fa
        H.assert $
            unFoldNatFree (foldNatFree fd :: forall x. m f x -> d x) fa
          ==
            fd fa
        H.assert $
            foldNatFree (unFoldNatFree mfd :: forall y. f y -> d y) mfa
          ==
            mfd mfa
        -- this is a consequence of the first property since
        -- `foldFree1 = foldNatFree id`
        H.assert $
            unFoldNatFree (foldFree1 @m) fa
          ==
            fa

prop_foldMapFree1_coyoneda :: Property
prop_foldMapFree1_coyoneda
    = foldMapFree1_property
        (genCoyoneda toOdd)
        (Gen.maybe $ Gen.integral @_ @Int (Range.linear 0 1000))
        id
        foldFree1

prop_foldMapFree1_ap :: Property
prop_foldMapFree1_ap
    = foldMapFree1_property
        (genAp (Gen.word8 (Range.linear 0 254)) genIntToInt)
        (Gen.maybe $ Gen.word8 (Range.linear 0 254))
        id
        foldFree1

prop_foldMapFree1_free :: Property
prop_foldMapFree1_free
    = foldMapFree1_property
        (genFree $ Gen.word8 (Range.linear 0 254))
        (Gen.maybe $ Gen.word8 (Range.linear 0 254))
        id
        foldFree1

foldFree1_property
    :: forall m f a
    .  ( FreeAlgebra1 m
       , AlgebraType m f
       , AlgebraType0 m f
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
    (H.forAllWith (show . Ap.retractAp) $ genAp (Gen.integral @_ @Int $ Range.linear 0 100) genIntToInt)
    Ap.retractAp

prop_foldFree1_free :: Property
prop_foldFree1_free = foldFree1_property
    (H.forAll $ genFree (Gen.integral @_ @Int $ Range.linear 0 100))
    (Free.foldFree id)

hoistFree1_property
    :: forall m f g a
    .  ( FreeAlgebra1 m
       , AlgebraType  m f
       , AlgebraType  m (m g)
       , AlgebraType0 m f
       , AlgebraType  m g
       , AlgebraType0 m g
       )
    => Gen (m f a)
    -> (m f a -> String)
    -> (m g a -> m g a -> Bool)
    -> (forall  x. f x -> g x)
    -> ((forall x. f x -> g x) -> m f a -> m g a)
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
    (genFree (Gen.integral @_ @Int $ Range.linear 0 100))
    show
    (==)
    (maybe (Left ()) Right)
    Free.hoistFree

iterFree1_property
    :: forall m f a
    .  ( FreeAlgebra1 m
       , AlgebraType  m f
       , AlgebraType0 m f
       , AlgebraType  m Identity
       , AlgebraType0 m Identity
       , Eq a
       , Show a
       )
    => Gen (m f a)
    -> (m f a -> String)
    -> (forall x.  f x -> x)
    -> ((forall x. f x -> x) -> m f a -> a)
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
    (\f -> Free.iter f)

prop_iterFree1_ap :: Property
prop_iterFree1_ap = iterFree1_property
    (genApIdentity (Gen.int $ Range.linear 0 1000) genIntToInt)
    (show . Ap.retractAp)
    runIdentity
    (\f -> Ap.iterAp f)

foldNatFree_liftFree_property
    :: forall m g f a proxy n.
       ( FreeAlgebra1 m
       , AlgebraType0 m f
       , AlgebraType  m g
       , AlgebraType0 m g
       , Eq (g a)
       , Monad n
       )
    => proxy m
    -> Gen (f a)
    -> (f a -> String)
    -> (forall x. f x -> g x)
    -> PropertyT n ()
foldNatFree_liftFree_property _ gen show_fa nat = do
    (fa :: f a) <- H.forAllWith show_fa gen
    H.assert $ (foldNatFree nat :: m f a -> g a) (liftFree fa :: m f a) == nat fa

prop_foldNatFree_liftFree_coyoneda :: Property
prop_foldNatFree_liftFree_coyoneda
    = property $ foldNatFree_liftFree_property
        (Proxy :: Proxy Coyoneda)
        (Gen.maybe $ Gen.word8 (Range.linear 0 255))
        show
        (maybe (Left ()) Right)

prop_foldNatFree_liftFree_ap :: Property
prop_foldNatFree_liftFree_ap
    = property $ foldNatFree_liftFree_property
        (Proxy :: Proxy Ap)
        (Gen.maybe $ Gen.word8 (Range.linear 0 255))
        show
        (maybe (Left ()) Right)

prop_foldNatFree_liftFree_free :: Property
prop_foldNatFree_liftFree_free
    = property $ foldNatFree_liftFree_property
        (Proxy :: Proxy Free)
        (Gen.maybe $ Gen.word8 (Range.linear 0 255))
        show
        (maybe (Left ()) Right)

foldNatFree_foldNatFree_property
    :: forall m f g h a proxy n.
       ( FreeAlgebra1 m
       , AlgebraType0 m f
       , AlgebraType0 m g
       , AlgebraType  m g
       , AlgebraType  m h
       , Monad n
       )
    => proxy m
    -> Gen (m f a)
    -> (m f a -> String)
    -> (h a -> h a -> Bool)
    -> (forall x. g x -> h x)
    -> (forall x. f x -> m g x)
    -> PropertyT n ()
foldNatFree_foldNatFree_property _ gen show_mfa eq_ha nat nat' =
    case codom1 :: Proof (AlgebraType m (m g)) (m g) of
      Proof -> do
          (mfa :: m f a) <- H.forAllWith show_mfa gen
          H.assert $ (foldNatFree nat :: m g a -> h a) (foldNatFree nat' mfa)
                  `eq_ha` foldNatFree (foldNatFree nat . nat') mfa

prop_foldNatFree_foldNatFree_coyoneda :: Property
prop_foldNatFree_foldNatFree_coyoneda = property $ do
    n <- H.forAll $ Gen.integral (Range.linear 0 255)
    foldNatFree_foldNatFree_property
      (Proxy :: Proxy Coyoneda)
      (genCoyoneda toOdd :: Gen (Coyoneda Maybe Int))
      show
      (==)
      (maybe (Left ()) Right)
      (liftFreeCoyonedaN n)
  where
    liftFreeCoyonedaN :: Int -> Maybe x -> Coyoneda Maybe x
    liftFreeCoyonedaN 0 fx = liftFree fx
    liftFreeCoyonedaN n fx = id <$> liftFreeCoyonedaN (pred n) fx

prop_foldNatFree_foldNatFree_ap :: Property
prop_foldNatFree_foldNatFree_ap
    = property $ do
        n  <- H.forAll $ Gen.integral (Range.linear 0 255)
        foldNatFree_foldNatFree_property
          (Proxy :: Proxy Ap)
          (genAp (Gen.integral (Range.linear 0 255)) genIntToInt :: Gen (Ap Maybe Int))
          (const "blind")
          (==)
          (maybe (Left ()) Right)
          (liftApN n id)
  where
    liftApN :: Int -> (x -> x) -> Maybe x -> Ap Maybe x
    liftApN 0 _ ma = Ap.liftAp ma
    liftApN n f ma = pure f <*> liftApN (pred n) f ma

prop_foldNatFree_foldNatFree_free :: Property
prop_foldNatFree_foldNatFree_free = property $ do
    n <- H.forAll $ Gen.integral (Range.linear 0 255)
    foldNatFree_foldNatFree_property
      (Proxy :: Proxy Free)
      (genFree (Gen.integral (Range.linear 0 255)) :: Gen (Free Maybe Int))
      show
      (==)
      (maybe (Left ()) Right)
      (liftFreeN n)
  where
    liftFreeN :: Int -> Maybe a -> Free Maybe a
    liftFreeN 0 ma = liftFree ma
    -- each bind wraps the lhs in a 'Free' constructor
    liftFreeN n ma = liftFreeN (pred n) ma >>= liftFree . Just

prop_foldNatFree_foldNatFree_StateT :: Property
prop_foldNatFree_foldNatFree_StateT = property $ do
    f1 <- H.forAll genIntToInt
    f2 <- H.forAll genIntToInt
    f3 <- H.forAll genIntToInt
    foldNatFree_foldNatFree_property
      (Proxy :: Proxy (S.StateT Int))
      gen
      (const "blind")
      (\s s' -> S.runStateT s 0 == S.runStateT s' 0)
      ((\s -> s >>= \x -> S.modify f1 >> pure x)
        :: forall x. S.StateT Int Identity x -> S.StateT Int Identity x)
      ((\(Identity x) -> S.lift (S.modify f2) >> S.modify f3 $> x)
        :: forall x. Identity x -> S.StateT Int (S.StateT Int Identity) x)
  where
    gen :: Gen (S.StateT Int Identity Int)
    gen = do
      a <- Gen.int $ Range.linear 0 9
      if a >= 3
        then do
          s <- Gen.int $ Range.linear 0 100
          x <- Gen.int $ Range.linear 0 100
          pure (S.put s $> x)
        else pure S.get

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
