{-# LANGUAGE UndecidableInstances #-}
module Data.Algebra.Pointed
    ( Pointed (..)
    ) where


-- |
-- Class of pointed sets
class Pointed p where
    point :: p

instance Pointed (Maybe a) where
    point = Nothing

instance Monoid m => Pointed m where
    point = mempty
