{-# LANGUAGE TemplateHaskell #-}
module Test.Data.Group.Free
    ( tests
    ) where

import           Data.Semigroup (Semigroup (..))
import           Data.Bool (bool)
import           Data.Group (invert)
import           Data.DList (DList)
import qualified Data.DList as DList
import           Hedgehog (Property, Gen, property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Data.Group.Free (fromDList, normalize, fromList, normalizeL)

genList :: Gen a -> Gen [Either a a]
genList gen =
    Gen.list (Range.linear 0 100) gen'
  where
    gen' = Gen.bool >>= bool (Right <$> gen) (Left <$> gen)

genDList :: Gen a -> Gen (DList (Either a a))
genDList gen = DList.fromList <$> genList gen

prop_normalize :: Property
prop_normalize = property $ do
    as <- H.forAll (genDList Gen.bool)

    normalize (normalize as) === normalize as
    normalize (as `DList.append` rev as) === DList.empty
  where
    rev :: DList (Either a a) -> DList (Either a a)
    rev = DList.foldr (\a as -> DList.snoc as (either Right Left a)) DList.empty

prop_invert :: Property
prop_invert = property $ do
    fg <- fromDList <$> H.forAll (genDList Gen.bool)

    invert (invert fg) === fg
    invert fg <> fg    === mempty
    fg <> invert fg    === mempty

prop_unit :: Property
prop_unit = property $ do
    fg <- fromDList <$> H.forAll (genDList Gen.bool)

    fg <> mempty === fg
    mempty <> fg === fg

prop_associativity :: Property
prop_associativity = property $ do
    fg   <- fromDList <$> H.forAll (genDList Gen.bool)
    fg'  <- fromDList <$> H.forAll (genDList Gen.bool)
    fg'' <- fromDList <$> H.forAll (genDList Gen.bool)

    (fg <> fg') <> fg'' === fg <> (fg' <> fg'')

prop_normalizeL :: Property
prop_normalizeL = property $ do
    as <- H.forAll (genList Gen.bool)

    normalizeL (normalizeL as) === normalizeL as
    normalizeL (as ++ rev as) === []
  where
    rev :: [Either a a] -> [Either a a]
    rev = foldl (\as a -> either Right Left a : as) []

prop_invertL :: Property
prop_invertL = property $ do
    fg <- fromList <$> H.forAll (genList Gen.bool)

    invert (invert fg) === fg
    invert fg <> fg    === mempty
    fg <> invert fg    === mempty

prop_unitL :: Property
prop_unitL = property $ do
    fg <- fromList <$> H.forAll (genList Gen.bool)

    fg <> mempty === fg
    mempty <> fg === fg

prop_associativityL :: Property
prop_associativityL = property $ do
    fg   <- fromList <$> H.forAll (genList Gen.bool)
    fg'  <- fromList <$> H.forAll (genList Gen.bool)
    fg'' <- fromList <$> H.forAll (genList Gen.bool)

    (fg <> fg') <> fg'' === fg <> (fg' <> fg'')

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
