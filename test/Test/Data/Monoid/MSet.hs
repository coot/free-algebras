{-# LANGUAGE TemplateHaskell #-}
module Test.Data.Monoid.MSet
    ( tests
    ) where

import Data.Functor.Identity
import Data.Monoid

import Data.Monoid.MSet

import           Hedgehog (Property, Gen, property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

mset_property :: forall m a.
                 ( Monoid m
                 , MSet m a
                 , Show a
                 , Eq a
                 )
              => Gen m
              -> (m -> String)
              -> Gen a
              -> Property
mset_property gens show_ gena = property $ do
  s1 <- H.forAllWith show_ gens
  s2 <- H.forAllWith show_ gens
  a  <- H.forAll gena

  s1 `mact` (s2 `mact` a) === (s1 <> s2) `mact` a
  mempty @m `mact` a === a

prop_mset_sum_int :: Property
prop_mset_sum_int =
  let gens :: Gen (S (Sum Int))
      gens = S . Sum <$> Gen.integral (Range.linear (-1024) 1024)
  in mset_property gens show gens

prop_mset_sum_functor :: Property
prop_mset_sum_functor =
  let gens :: Gen (Sum Int)
      gens = Sum <$> Gen.integral (Range.linear (-1024) 1024)
      gena :: Gen (Identity Int)
      gena = Identity <$> Gen.integral (Range.linear (-1024) 1024)
  in mset_property gens show gena

prop_mset_endo :: Property
prop_mset_endo =
  let gens :: Gen (Endo (Sum Int))
      gens = Endo . (<>) . Sum <$> Gen.integral (Range.linear (-1024) 1024)
      gena :: Gen (Sum Int)
      gena = Sum <$> Gen.integral (Range.linear (-1024) 1024)
  in mset_property gens (const "*") gena

prop_mset_s_sum_int :: Property
prop_mset_s_sum_int =
  let gens :: Gen (Sum Int)
      gens = Sum <$> Gen.integral (Range.linear (-1024) 1024)
      gena :: Gen Int
      gena = Gen.integral (Range.linear (-1024) 1024)
  in mset_property gens show gena

prop_mset_endo2 :: Property
prop_mset_endo2 =
  let gens :: Gen (S (Sum Int))
      gens = S . Sum <$> Gen.integral (Range.linear (-1024) 1024)
      gena :: Gen (Endo Int)
      gena = Endo . (+) <$> Gen.integral (Range.linear (-1024) 1024)
      genb :: Gen Int
      genb = Gen.integral (Range.linear (-1024) 1024)
  in property $ do
    s1 <- H.forAll gens
    s2 <- H.forAll gens
    a  <- H.forAllWith (const "") gena
    b  <- H.forAll genb
    act (s1 <> s2) a `appEndo` b === act s1 (act s2 a) `appEndo` b

prop_mset_product :: Property
prop_mset_product =
  let gens :: Gen (Product Int)
      gens = Product <$> Gen.integral (Range.linear (-1024) 1024)
      gena :: Gen Int
      gena = Gen.integral (Range.linear (-1024) 1024)
  in mset_property gens show gena

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
