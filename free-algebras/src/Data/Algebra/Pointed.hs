{-# LANGUAGE UndecidableInstances #-}
module Data.Algebra.Pointed
    ( Pointed (..)
    , AlgebraType
    ) where


import           Data.Algebra.Free (AlgebraType, FreeAlgebra (..))

-- |
-- Class of pointed sets
class Pointed p where
    point :: p

instance Pointed (Maybe a) where
    point = Nothing

instance Monoid m => Pointed m where
    point = mempty

type instance AlgebraType Maybe m = Pointed m
instance FreeAlgebra Maybe where
    returnFree = Just
    foldMapFree f (Just a) = f a
