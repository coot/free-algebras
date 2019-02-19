{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}

-- |
-- A type class for free objects of kind @k -> k -> Type@, i.e. /graphs/ (we
-- will use this name for types of this kind in this documentation).  Examples
-- include various flavors of /free categories/ and /arrows/ which
-- are not included in this package, see
-- __[free-category](https://hackage.haskell.org/package/free-category)__ on
-- /Hackage/).
--
module Control.Algebra.Free2
    ( -- * Free algebra class
      FreeAlgebra2 (..)
      -- ** Type level witnesses
    , Proof (..)
    , proof
      -- ** Algebra types \/ constraints
    , AlgebraType0
    , AlgebraType
      -- * Combinators
    , wrapFree2
    , foldFree2
    , unFoldNatFree2
    , hoistFree2
    , hoistFreeH2
    , joinFree2
    , bindFree2
    , assocFree2
    ) where

import           Control.Monad (join)
import           Data.Constraint (Dict (..))
import           Data.Kind (Type)

import           Data.Algebra.Free (AlgebraType, AlgebraType0, Proof (..), proof)

-- |
-- Free algebra class similar to @'FreeAlgebra1'@ and @'FreeAlgebra'@, but for
-- types of kind @k -> k -> Type@.
--
class FreeAlgebra2 (m :: (k -> k -> Type) -> k -> k -> Type) where

    -- |
    -- Lift a graph @f@ satsifying the constraint @'AlgebraType0'@ to
    -- a free its object @m f@.
    --
    liftFree2    :: AlgebraType0 m f
                 => f a b
                 -> m f a b

    -- |
    -- This represents the theorem that @m f@ is indeed free object (as
    -- in propositions as types).  The types of kind @k -> k -> Type@ form
    -- a category, where an arrow from @f :: k -> k -> Type@ to @d :: k ->
    -- k -> Type@ is represented by type @forall x y. f x y -> d x y@.
    -- @foldNatFree2@ states that whenever we have such a morphism and @d@
    -- satisfies the constraint @AlgebraType m d@ then we can construct
    -- a morphism from @m f@ to @d@.
    --
    foldNatFree2 :: forall (d :: k -> k -> Type)
                           (f :: k -> k -> Type) a b .
                    ( AlgebraType  m d
                    , AlgebraType0 m f
                    )
                 => (forall x y. f x y -> d x y)
                 -> (m f a b -> d a b)

    -- |
    -- A proof that for each @f@ satisfying @AlgebraType0 m f@, @m f@
    -- satisfies @AlgebraType m (m f)@ constrant.  This means that @m@ is
    -- a well defined /functor/ from the full sub-category of types of
    -- kind @k -> k -> Type@ which satisfy the @AlgebraType0 m@ constraint
    -- to the full subcategory of types of the same kind which satifsfy
    -- the constraint @AlgebraType m@.
    --
    codom2  :: forall (f :: k -> k -> Type).
               AlgebraType0 m f
            => Proof (AlgebraType m (m f)) (m f)

    -- | 
    -- A proof that each type @f :: k -> k -> Type@ satisfying the
    -- @Algebra m f@ constraint also satisfies @AlgebraType0 m f@.  This
    -- states that there is a well defined /forgetful functor/ from the
    -- category of types of kind @k -> k -> Type@ which satisfy the
    -- @AlgebraType m@ to the category of types of the same kind which
    -- satisfy the @AlgebraType0 m@ constraint.
    --
    forget2 :: forall (f :: k -> k -> Type).
               AlgebraType  m f
            => Proof (AlgebraType0 m f) (m f)

--
-- Combinaators
--

-- | A version of @wrap@ from __free__ package but for graphs.
--
wrapFree2 :: forall (m :: (Type -> Type -> Type) -> Type -> Type -> Type)
                    (f :: Type -> Type -> Type)
                    a b .
             ( AlgebraType0 m f
             , FreeAlgebra2 m
             , Monad (m f a)
             )
          => f a (m f a b)
          -> m f a b
wrapFree2 = join . liftFree2
{-# INLINE wrapFree2 #-}

-- | Like @'foldFree'@ or @'foldFree1'@ but for graphs.
-- 
-- A lawful instance will satisfy:
--
-- @
--  'foldFree2' . 'liftFree2' == 'id' :: f a b -> f a b
-- @
--
-- It is the [unit](https://ncatlab.org/nlab/show/unit+of+an+adjunction) of
-- adjuction defined by @'FreeAlgebra1'@ class.
--
foldFree2 :: forall (m :: (k -> k -> Type) -> k -> k -> Type)
                    (f :: k -> k -> Type)
                    a b .
             ( FreeAlgebra2 m
             , AlgebraType  m f
             )
          => m f a b
          -> f a b
foldFree2 = case forget2 :: Proof (AlgebraType0 m f) (m f) of
    Proof Dict -> foldNatFree2 id
{-# INLINE foldFree2 #-}

-- | 
-- Inverse of @'foldNatFree2'@.
--
-- It is uniquelly determined by its universal property (by Yonneda lemma):
--
-- prop> unFoldNatFree id = liftFree2
--
unFoldNatFree2
    :: forall (m :: (k -> k -> Type) -> k -> k -> Type)
              (f :: k -> k -> Type)
              d a b.
       ( FreeAlgebra2 m
       , AlgebraType0 m f
       )
    => (forall x y. m f x y -> d x y)
    -> f a b -> d a b
unFoldNatFree2 nat = nat . liftFree2
{-# INLINE unFoldNatFree2 #-}

-- |
-- Hoist the underlying graph in the free structure.
-- This is a higher version of a functor (analogous to @'fmapFree'@, which
-- defined functor instance for @'FreeAlgebra'@ instances) and it satisfies the
-- functor laws:
--
-- prop> hoistFree2 id = id
-- prop> hoistFree2 f . hoistFree2 g = hoistFree2 (f . g)
--
hoistFree2 :: forall (m :: (k -> k -> Type) -> k -> k -> Type)
                     (f :: k -> k -> Type)
                     g a b .
              ( FreeAlgebra2 m
              , AlgebraType0 m g
              , AlgebraType0 m f
              )
           => (forall x y. f x y -> g x y)
           -> m f a b
           -> m g a b
hoistFree2 nat = case codom2 :: Proof (AlgebraType m (m g)) (m g) of
    Proof Dict -> foldNatFree2 (liftFree2 . nat)
{-# INLINE hoistFree2 #-}

-- |
-- Hoist the top level free structure.
--
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

-- |
-- @'FreeAlgebra2' m@ is a monad on some subcategory of graphs (types of kind
-- @k -> k -> Type@), @'joinFree'@ it is the @join@ of this monad.
--
joinFree2 :: forall (m :: (k -> k -> Type) -> k -> k -> Type)
                    (f :: k -> k -> Type)
                    a b .
             ( FreeAlgebra2 m
             , AlgebraType0 m f
             )
          => m (m f) a b
          -> m f a b
joinFree2 = case codom2 :: Proof (AlgebraType m (m f)) (m f) of
    Proof Dict -> case forget2 :: Proof (AlgebraType0 m (m f)) (m (m f)) of
        Proof Dict -> foldFree2
{-# INLINE joinFree2 #-}

-- |
-- @bind@ of the monad defined by @m@ on the subcategory of graphs (typed of
-- kind @k -> k -> Type@).
--
bindFree2 :: forall m f g a b .
             ( FreeAlgebra2 m
             , AlgebraType0 m g
             , AlgebraType0 m f
             )
          => m f a b
          -> (forall x y . f x y -> m g x y)
          -> m g a b
bindFree2 mfa nat = case codom2 :: Proof (AlgebraType m (m g)) (m g) of
    Proof Dict -> foldNatFree2 nat mfa
{-# INLINE bindFree2 #-}

assocFree2 :: forall (m :: (Type -> Type -> Type) -> Type -> Type -> Type)
                     (f :: Type -> Type -> Type)
                     a b .
              ( FreeAlgebra2 m
              , AlgebraType  m f
              , Functor (m (m f) a)
              )
           => m f a (m f a b)
           -> m (m f) a (f a b)
assocFree2 = case forget2 :: Proof (AlgebraType0 m f) (m f) of
    Proof Dict -> case codom2 :: Proof (AlgebraType m (m f)) (m f) of
        Proof Dict -> case forget2 :: Proof (AlgebraType0 m (m f)) (m (m f)) of
            Proof Dict -> case codom2 :: Proof (AlgebraType m (m (m f))) (m (m f)) of
                Proof Dict -> fmap foldFree2 <$> foldNatFree2 (hoistFree2 liftFree2 . liftFree2)
{-# INLINE assocFree2 #-}
