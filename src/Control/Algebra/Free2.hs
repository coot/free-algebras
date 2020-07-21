{-# LANGUAGE DefaultSignatures   #-}
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
import           Data.Kind (Type)

import           Data.Algebra.Free (AlgebraType, AlgebraType0, Proof (..))

-- |
-- Free algebra class similar to @'FreeAlgebra1'@ and @'FreeAlgebra'@, but for
-- types of kind @k -> k -> Type@.
--
class FreeAlgebra2 (m :: (k -> k -> Type) -> k -> k -> Type) where

    {-# MINIMAL liftFree2, foldNatFree2 #-}

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
    -- prop> foldNatFree2 nat (liftFree2 tr) = nat tr
    -- prop> foldNatFree2 nat . foldNatFree2 nat' = foldNatFree2 (foldNatFree2 nat . nat')
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

    default codom2 :: forall a. AlgebraType m (m a)
                   => Proof (AlgebraType m (m a)) (m a)
    codom2 = Proof

    -- | 
    -- A proof that each type @f :: k -> k -> Type@ satisfying the
    -- @Algebra m f@ constraint also satisfies @AlgebraType0 m f@.  This
    -- states that there is a well defined /forgetful functor/ from the
    -- category of types of kind @k -> k -> Type@ which satisfy the
    -- @AlgebraType m@ to the category of types of the same kind which
    -- satisfy the @AlgebraType0 m@ constraint.
    --
    forget2 :: forall (f :: k -> k -> Type).
               AlgebraType m f
            => Proof (AlgebraType0 m f) (m f)

    default forget2 :: forall a. AlgebraType0 m a
                    => Proof (AlgebraType0 m a) (m a)
    forget2 = Proof

-- Combinators
--

-- | Version of @wrap@ from @free@ package but for graphs.
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
{-# INLINABLE wrapFree2 #-}

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
foldFree2 :: forall k
                    (m :: (k -> k -> Type) -> k -> k -> Type)
                    (f :: k -> k -> Type)
                    a b .
             ( FreeAlgebra2 m
             , AlgebraType  m f
             )
          => m f a b
          -> f a b
foldFree2 = case forget2 :: Proof (AlgebraType0 m f) (m f) of
    Proof -> foldNatFree2 id
{-# INLINABLE foldFree2 #-}

-- | 
-- Inverse of @'foldNatFree2'@.
--
-- It is uniquelly determined by its universal property (by Yonneda lemma):
--
-- prop> unFoldNatFree id = liftFree2
--
unFoldNatFree2
    :: forall k
              (m :: (k -> k -> Type) -> k -> k -> Type)
              (f :: k -> k -> Type)
              d a b.
       ( FreeAlgebra2 m
       , AlgebraType0 m f
       )
    => (forall x y. m f x y -> d x y)
    -> f a b -> d a b
unFoldNatFree2 nat = nat . liftFree2
{-# INLINABLE unFoldNatFree2 #-}

-- |
-- Hoist the underlying graph in the free structure.
-- This is a higher version of a functor (analogous to @'fmapFree'@, which
-- defined functor instance for @'FreeAlgebra'@ instances) and it satisfies the
-- functor laws:
--
-- prop> hoistFree2 id = id
-- prop> hoistFree2 f . hoistFree2 g = hoistFree2 (f . g)
--
hoistFree2 :: forall k
                     (m :: (k -> k -> Type) -> k -> k -> Type)
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
    Proof -> foldNatFree2 (liftFree2 . nat)
{-# INLINABLE [1] hoistFree2 #-}

{-# RULES

"hositFree2/foldNatFree2"
    forall (nat  :: forall x y. g x y -> c x y)
           (nat0 :: forall x y. f x y -> g x y)
           (f :: m f a b).
    foldNatFree2 nat (hoistFree2 nat0 f) = foldNatFree2 (nat . nat0) f

#-}

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
{-# INLINABLE [1] hoistFreeH2 #-}

{-# RULES

"hoistFreeH2/foldNatFree2" forall (nat :: forall x y. f x y -> c x y)
                                  (f :: AlgebraType m c => m f a b).
                           foldNatFree2 nat (hoistFreeH2 f) = foldNatFree2 nat f
#-}

-- | 'FreeAlgebra2' m@ is a monad on some subcategory of graphs (types of kind
-- @k -> k -> 'Type'@), 'joinFree' it is the 'join' of this monad.
--
-- prop> foldNatFree2 nat . joinFree2 = foldNatFree2 (foldNatFree2 nat)
--
-- This property is analogous to @foldMap f . concat = foldMap (foldMap f)@,
--
joinFree2 :: forall k
                    (m :: (k -> k -> Type) -> k -> k -> Type)
                    (f :: k -> k -> Type)
                    a b .
             ( FreeAlgebra2 m
             , AlgebraType0 m f
             )
          => m (m f) a b
          -> m f a b
joinFree2 = case codom2 :: Proof (AlgebraType m (m f)) (m f) of
    Proof -> case forget2 :: Proof (AlgebraType0 m (m f)) (m (m f)) of
        Proof -> foldFree2
{-# INLINABLE joinFree2 #-}

-- |
-- @bind@ of the monad defined by @m@ on the subcategory of graphs (types of
-- kind @k -> k -> Type@).
--
-- prop> foldNatFree2 nat (bindFree mf nat') = foldNatFree2 (foldNatFree2 nat . nat') mf
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
    Proof -> foldNatFree2 nat mfa
{-# INLINABLE bindFree2 #-}

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
    Proof -> case codom2 :: Proof (AlgebraType m (m f)) (m f) of
        Proof -> case forget2 :: Proof (AlgebraType0 m (m f)) (m (m f)) of
            Proof -> case codom2 :: Proof (AlgebraType m (m (m f))) (m (m f)) of
                Proof -> fmap foldFree2 . foldNatFree2 (hoistFree2 liftFree2 . liftFree2)
{-# INLINABLE assocFree2 #-}
