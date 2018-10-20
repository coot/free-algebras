{-# LANGUAGE TemplateHaskell #-}

module Test.Data.Semigroup.SSet where

import Data.Functor.Identity
import Data.Semigroup

import Data.Semigroup.SSet

import           Hedgehog (Property, Gen, property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

sset_property :: ( Semigroup s
                 , SSet s a
                 , Show a
                 , Eq a
                 )
              => Gen s
              -> (s -> String)
              -> Gen a
              -> Property
sset_property gens show_ gena = property $ do
  s1 <- H.forAllWith show_ gens
  s2 <- H.forAllWith show_ gens
  a  <- H.forAll gena
  s1 `act` (s2 `act` a) === (s1 <> s2) `act` a

prop_sset_sum_int :: Property
prop_sset_sum_int =
  let gens :: Gen (S (Sum Int))
      gens = S . Sum <$> Gen.integral (Range.linear (-1024) 1024)
  in sset_property gens show gens

_s2 :: Identity (S (Sum Int))
_s2 = act @(S (Sum Int)) (S (Sum 1)) (Identity (S (Sum 2)))

prop_sset_sum_functor :: Property
prop_sset_sum_functor =
  let gens :: Gen (Sum Int)
      gens = Sum <$> Gen.integral (Range.linear (-1024) 1024)
      gena :: Gen (Identity Int)
      gena = Identity <$> Gen.integral (Range.linear (-1024) 1024)
  in sset_property gens show gena

prop_sset_endo :: Property
prop_sset_endo =
  let gens :: Gen (Endo (Sum Int))
      gens = Endo . (<>) . Sum <$> Gen.integral (Range.linear (-1024) 1024)
      gena :: Gen (Sum Int)
      gena = Sum <$> Gen.integral (Range.linear (-1024) 1024)
  in sset_property gens (const "*") gena

prop_sset_s_sum_int :: Property
prop_sset_s_sum_int =
  let gens :: Gen (Sum Int)
      gens = Sum <$> Gen.integral (Range.linear (-1024) 1024)
      gena :: Gen Int
      gena = Gen.integral (Range.linear (-1024) 1024)
  in sset_property gens show gena

prop_sset_endo2 :: Property
prop_sset_endo2 =
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

prop_sset_product :: Property
prop_sset_product =
  let gens :: Gen (Product Int)
      gens = Product <$> Gen.integral (Range.linear (-1024) 1024)
      gena :: Gen Int
      gena = Gen.integral (Range.linear (-1024) 1024)
  in sset_property gens show gena

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
