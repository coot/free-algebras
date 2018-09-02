{-# LANGUAGE TemplateHaskell #-}
module Test.Data.Group.Free
    ( tests
    ) where

import           Control.Monad (mapM)
import           Data.Group (invert)
import           Data.DList (DList)
import qualified Data.DList as DList
import           Hedgehog (Property, PropertyT, Gen, property, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Data.Group.Free (FreeGroup, fromDList, normalize)

genDList :: Gen a -> Gen (DList (Either a a))
genDList gen = do
    as <- Gen.list (Range.linear 0 100) gen
    DList.fromList <$> mapM
        (\a -> do
            b <- Gen.bool
            if b
            then return $ Right a
            else return $ Left a
        )
        as

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

    fg <> mempty       === fg
    mempty <> fg       === fg

prop_associativity :: Property
prop_associativity = property $ do
    fg   <- fromDList <$> H.forAll (genDList Gen.bool)
    fg'  <- fromDList <$> H.forAll (genDList Gen.bool)
    fg'' <- fromDList <$> H.forAll (genDList Gen.bool)

    (fg <> fg') <> fg'' === fg <> (fg' <> fg'')

tests :: IO Bool
tests = H.checkParallel $$(H.discover)
