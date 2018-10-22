module Control.Algebra.Free2
    ( FreeAlgebra2 (..)
    , Proof (..)
    , proof
    , AlgebraType0
    , AlgebraType
    , wrapFree2
    , foldFree2
    , unFoldNatFree2
    , hoistFree2
    , hoistFreeH2
    , joinFree2
    , bindFree2
    ) where

import           Control.Monad (join)
import           Data.Constraint (Dict (..))
import           Data.Kind (Type)

import           Data.Algebra.Free (AlgebraType, AlgebraType0, Proof (..), proof)

class FreeAlgebra2 (m :: (Type -> Type -> Type) -> Type -> Type -> Type) where
    liftFree2    :: AlgebraType0 m f => f a b -> m f a b
    foldNatFree2 :: forall d f a b .
                    ( AlgebraType  m d
                    , AlgebraType0 m f
                    )
                 => (forall x y. f x y -> d x y)
                 -> (m f a b -> d a b)

    codom2  :: forall f. AlgebraType0 m f => Proof (AlgebraType m (m f)) (m f)
    forget2 :: forall f. AlgebraType  m f => Proof (AlgebraType0 m f) (m f)

wrapFree2 :: forall m f a b .
             ( AlgebraType0 m f
             , FreeAlgebra2 m
             , Monad (m f a)
             )
          => f a (m f a b)
          -> m f a b
wrapFree2 = join . liftFree2
{-# INLINE wrapFree2 #-}

foldFree2 :: forall m f a b .
             ( FreeAlgebra2 m
             , AlgebraType  m f
             )
          => m f a b
          -> f a b
foldFree2 = case forget2 @m @f of
    Proof Dict -> foldNatFree2 id
{-# INLINE foldFree2 #-}

unFoldNatFree2
    :: ( FreeAlgebra2 m
       , AlgebraType0 m f
       )
    => (forall x y. m f x y -> d x y)
    -> f a b -> d a b
unFoldNatFree2 nat = nat . liftFree2
{-# INLINE unFoldNatFree2 #-}

hoistFree2 :: forall m f g a b .
              ( FreeAlgebra2 m
              , AlgebraType0 m g
              , AlgebraType0 m f
              )
           => (forall x y. f x y -> g x y) -- ^ a functor from @f@ to @g@
           -> m f a b
           -> m g a b
hoistFree2 nat = case codom2 @m @g of
    Proof Dict -> foldNatFree2 (liftFree2 . nat)
{-# INLINE hoistFree2 #-}

hoistFreeH2 :: forall m n f a b .
           ( FreeAlgebra2 m
           , FreeAlgebra2 n
           , AlgebraType0 m f
           , AlgebraType0 n f
           , AlgebraType  m (n f)
           )
        => m f a b
        -> n f a b
hoistFreeH2 = foldNatFree2 liftFree2
{-# INLINE hoistFreeH2 #-}

joinFree2 :: forall m f a b .
             ( FreeAlgebra2 m
             , AlgebraType0 m f
             )
          => m (m f) a b
          -> m f a b
joinFree2 = case codom2 @m @f of
    Proof Dict -> case forget2 @m @(m f) of
        Proof Dict -> foldFree2
{-# INLINE joinFree2 #-}

bindFree2 :: forall m f g a b .
             ( FreeAlgebra2 m
             , AlgebraType0 m g
             , AlgebraType0 m f
             )
          => m f a b
          -> (forall x y . f x y -> m g x y) -- ^ functor from @f@ to @g@
          -> m g a b
bindFree2 mfa nat = case codom2 @m @g of
    Proof Dict -> foldNatFree2 nat mfa
{-# INLINE bindFree2 #-}
