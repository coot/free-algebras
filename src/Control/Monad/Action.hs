{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Monad.Action where

import           Control.Monad (join)
import           Data.Functor.Const (Const (..))

import           Control.Algebra.Free (AlgebraType0, AlgebraType, FreeAlgebra1 (..))
import           Data.Algebra.Free (FreeAlgebra, foldFree)

-- |
-- A monad action is an `m`-algebra parametrized over a functor `f`.
-- This is direct translation of a monoid action in the monoidal category of
-- endofunctors with monoidal product: functor composition.
class (Monad m, Functor f) => MAction m f where
    mact :: m (f a) -> f a

instance Monad m => MAction m m where
    mact = join

-- |
-- Every algebra @d@ which satisfies the constraint @'AlgebraType' m d@ lifts
-- to an action on the constant functor @'Const' d@.  This is the same as to
-- say that @d@ is an @m@-algebra (as of f-algebras in category theory).
instance (Monad m, FreeAlgebra m, AlgebraType m d) => MAction m (Const d) where
    mact mca = Const $ foldFree $ getConst <$> mca

newtype FreeMAction m f a = FreeMAction { runFreeMAction :: m (f a) }
    deriving (Functor)

instance (Monad m, Functor f) => MAction m (FreeMAction m f) where
    mact mfa = FreeMAction $ join $ runFreeMAction <$> mfa

type instance AlgebraType  (FreeMAction m) f = MAction m f
type instance AlgebraType0 (FreeMAction m) f = Functor f
instance Monad m => FreeAlgebra1 (FreeMAction m) where
    liftFree = FreeMAction . return
    foldNatFree nat (FreeMAction mfa) = mact $ nat <$> mfa

