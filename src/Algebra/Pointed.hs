{-# LANGUAGE UndecidableInstances #-}
module Algebra.Pointed
    ( Pointed (..)
    , AlgebraType
    ) where


import           Algebra.Free.Class (AlgebraType, FreeAlgebra (..))

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
    returnF = Just
    foldMapF f (Just a) = f a
