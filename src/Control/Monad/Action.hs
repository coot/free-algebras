{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Control.Monad.Action where

import           Control.Monad (join)
import           Data.Functor.Const (Const (..))
import           Data.Functor.Compose (Compose (..))
import           Data.Kind (Type)

import           Control.Algebra.Free
                    ( AlgebraType0
                    , AlgebraType
                    , FreeAlgebra1 (..)
                    )
import           Data.Algebra.Pointed (Pointed (point))
import           Data.Algebra.Free (FreeAlgebra, foldFree)

-- | A /monad action/ is an `m`-algebra parametrized over a functor `f`.
-- This is direct translation of a /monoid action/ in the monoidal category of
-- endofunctors with monoidal product: functor composition.
--
-- @'mact'@ should be /associative/:
-- prop> 'mact' . 'mact' = 'mact' . 'join'
-- and /unital/:
-- prop> mact . return = id
--
-- There are monads which do not have any (safe) instances, like @'IO'@.
--
class (Monad m, Functor f) => MAction m f where
    mact :: m (f a) -> f a

instance Monad m => MAction m m where
    mact = join

-- | You can use @'PointedMonoid'@ newtype wrapper if you want to laverage
-- @'Pointed'@ instance for a @'Monoid'@.
--
instance (Pointed r, Functor f) => MAction ((->) r) f where
    mact f = f point

-- | Every algebra @d@ which satisfies the constraint @'AlgebraType' m d@ lifts
-- to an action on the constant functor @'Const' d@.  This is the same as to
-- say that @d@ is an @m@-algebra (as of /f-algebras/ in category theory).
--
instance ( Monad m
         , FreeAlgebra  m
         , AlgebraType  m d
         )
         => MAction m (Const d) where
    mact mca = Const $ foldFree $ getConst <$> mca

-- | Free algebra associated with the @'MAction' constraint.
--
newtype FreeMAction (m :: Type -> Type) (f :: Type -> Type) a =
    FreeMAction {
        runFreeMAction :: m (f a)
    }
    deriving (Show, Eq, Ord, Functor)

instance (Applicative m, Applicative f) => Applicative (FreeMAction m f) where

    pure = FreeMAction . getCompose . pure

    FreeMAction mfa <*> FreeMAction mfb =
        FreeMAction $ getCompose $ Compose mfa <*> Compose mfb


instance (Monad m, Functor f) => MAction m (FreeMAction m f) where

    mact mfa = FreeMAction $ mfa >>= runFreeMAction


type instance AlgebraType  (FreeMAction m) f = MAction m f
type instance AlgebraType0 (FreeMAction m) f = Functor f
instance Monad m => FreeAlgebra1 (FreeMAction m) where
    liftFree = FreeMAction . return
    foldNatFree nat (FreeMAction mfa)
             = mact $ nat <$> mfa
