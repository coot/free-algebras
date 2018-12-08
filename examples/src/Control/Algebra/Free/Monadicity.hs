{-# LANGUAGE GADTs #-}
module Control.Algebra.Free.Monadicity
    ( Hom (..)
    , unHom
    , idHom
    , composeHom
    , bimapHom
    , AlgHom (..)
    , unAlgHom
    , idAlgHom
    , composeAlgHom
    , bimapAlgHom
    , forget_
    , psi
    , phi
    , unit
    , counit
    , FreeMAlg (..)
    , runFreeMAlg
    , fmapF
    , returnF
    , joinF
    , joinF'
    , bindF
    , MAlg (..)
    -- , algfn
    , algFreeMAlg
    , returnFreeMAlg
    , foldMapFreeMAlg
    , foldFreeMAlg
    , k
    )where

import           Prelude

import           Data.Constraint (Dict (..))
import           Data.Proxy (Proxy (..))
import           Data.Kind (Type)

import           Control.Algebra.Free

data Hom m (a :: * -> *) (b :: * -> *) where
    Hom :: ( AlgebraType0 m a, AlgebraType0 m b )
        => (forall x. a x -> b x)
        -> Hom m a b

unHom :: Hom m a b -> a x -> b x
unHom (Hom f) = f

idHom :: AlgebraType0 m a => Hom m a a
idHom = Hom id

composeHom :: Hom m b c -> Hom m a b -> Hom m a c
composeHom (Hom f) (Hom g) = Hom (f . g)

bimapHom :: forall m a' a b b'.
            ( AlgebraType0 m a'
            , AlgebraType0 m b'
            )
         => (forall x. a' x -> a x)
         -> (forall x. b x  -> b' x)
         -> Hom m a b
         -> Hom m a' b'
bimapHom f g (Hom ab) = Hom (g . ab . f)

data AlgHom m a b where
    AlgHom :: ( AlgebraType  m a
              , AlgebraType  m b
              )
           => (forall x. a x -> b x)
           -> AlgHom m a b

unAlgHom :: AlgHom m a b -> a x -> b x
unAlgHom (AlgHom f) = f

forget_ :: forall (m :: (Type -> Type) -> Type -> Type) a b .
           FreeAlgebra1 m
        => AlgHom m a b
        -> Hom m a b
forget_ (AlgHom f) = case forget1 :: Proof (AlgebraType0 m a) (m a) of
    Proof Dict -> case forget1 :: Proof (AlgebraType0 m b) (m b) of
        Proof Dict -> Hom f

idAlgHom :: AlgebraType m a => AlgHom m a a
idAlgHom = AlgHom id

-- |
-- @'AlgHom'@ is a category
composeAlgHom :: AlgHom m b c -> AlgHom m a b -> AlgHom m a c
composeAlgHom (AlgHom f) (AlgHom g) = AlgHom (f . g)

bimapAlgHom :: forall m a' a b b'.
               ( AlgebraType  m a'
               , AlgebraType  m b'
               )
            => (forall x. a' x -> a x)
            -> (forall x. b x  -> b' x)
            -> AlgHom m a b
            -> AlgHom m a' b'
bimapAlgHom f g (AlgHom ab) = AlgHom (g . ab . f)

-- |
-- @ψ :: (...) AlgHom m (m a) d -> Hom m a d@
-- with inverse @'phi'@.
psi :: forall m a d .
         ( FreeAlgebra1 m
         , AlgebraType0 m a
         )
      => AlgHom m (m a) d
      -> Hom m a d
psi (AlgHom f) = case forget1 :: Proof (AlgebraType0 m d) (m d) of
    Proof Dict -> Hom $ unFoldNatFree f

-- |
-- @φ :: (...) => Hom m a d -> AlgHom m (m a) d@
-- with inverse of @'psi'@
phi :: forall m a d .
        ( FreeAlgebra1 m
        , AlgebraType  m d
        , AlgebraType0 m a
        )
     => Hom m a d
     -> AlgHom m (m a) d
phi (Hom f) = case codom1 :: Proof (AlgebraType m (m a)) (m a) of
    Proof Dict -> case forget1 :: Proof (AlgebraType0 m (m a)) (m (m a)) of
        Proof Dict -> AlgHom $ foldNatFree f

-- |
-- [unit](https://en.wikipedia.org/wiki/Adjoint_functors#Definition_via_counit%E2%80%93unit_adjunction)
-- of the adjunction, which turns out to be @'returnFree'@.
unit :: forall m a .
        ( FreeAlgebra1 m
        , AlgebraType0 m a
        )
     => Hom m a (m a)
unit = case codom1 :: Proof (AlgebraType m (m a)) (m a) of
    Proof Dict -> case forget1 :: Proof (AlgebraType0 m (m a)) (m (m a)) of
        Proof Dict -> psi (AlgHom id)

-- |
-- [counit](https://en.wikipedia.org/wiki/Adjoint_functors#Definition_via_counit%E2%80%93unit_adjunction)
-- of the adjunction, which boils down to @'foldMapFree' id@.
counit :: forall (m :: (Type -> Type) -> Type -> Type) d . 
          ( FreeAlgebra1 m
          , AlgebraType  m d
          )
       => AlgHom m (m d) d
counit = case forget1 :: Proof (AlgebraType0 m d) (m d) of
    Proof Dict -> phi (Hom id)

-- |
-- The monad associated with the adjunction.  Note that it's isomorphic to
-- @'FreeAlgebra1' m => m a@.
data FreeMAlg (m :: (* -> *) -> * -> *) (f :: * -> *) (a :: *) where
    FreeMAlg :: (FreeAlgebra1 m, AlgebraType0 m f) => m f a -> FreeMAlg m f a

runFreeMAlg :: FreeMAlg m f a -> m f a
runFreeMAlg (FreeMAlg mfa) = mfa

-- |
-- @'FreeMAlg'@ is a functor in the category @Hom m@.
fmapF :: forall m f g a .
         Hom m f g
      -> FreeMAlg m f a
      -> FreeMAlg m g a
fmapF (Hom fn) (FreeMAlg mfa) = FreeMAlg $ hoistFree1 fn mfa

-- |
-- unit of the @'FreeMAlg'@ monad (i.e. @return@ in Haskell)
returnF :: forall m f .
           ( FreeAlgebra1 m
           , AlgebraType0 m f
           , AlgebraType0 m (FreeMAlg m f)
           )
        => Hom m f (FreeMAlg m f)
returnF = case unit :: Hom m f (m f) of Hom f -> Hom (FreeMAlg . f)

-- |
-- join of the @'FreeMAlg'@ monad
joinF :: forall  m f .
         ( FreeAlgebra1 m
         , AlgebraType0 m f
         , AlgebraType0 m (FreeMAlg m f)
         , AlgebraType0 m (FreeMAlg m (FreeMAlg m f))
         )
      => Hom m (FreeMAlg m (FreeMAlg m f)) (FreeMAlg m f)
joinF = case codom1 :: Proof (AlgebraType m (m f)) (m f) of
    Proof Dict -> case forget1 :: Proof (AlgebraType0 m (m f)) (m (m f)) of
        Proof Dict -> Hom $ \(FreeMAlg mma) -> FreeMAlg $ joinFree1 $ hoistFree1 runFreeMAlg mma


-- |
-- The same as @'joinF'@ but defined the same way as in categor theory text
-- books where newtype wrapers do not show up ;).
joinF' :: forall  m a .
         ( FreeAlgebra1 m
         , AlgebraType0 m a
         )
      => Hom m (m (m a)) (m a)
joinF' = case codom1 :: Proof (AlgebraType m (m a)) (m a) of
    Proof Dict -> forget_ counit

-- |
-- bind of the @'FreeMAlg'@ monad
bindF :: forall m f g a .
         ( FreeAlgebra1 m
         , AlgebraType0 m g
         )
      => FreeMAlg m f a
      -> Hom m f (FreeMAlg m g)
      -> FreeMAlg m g a
bindF (FreeMAlg ma) (Hom f) = case codom1 :: Proof (AlgebraType m (m f)) (m f) of
    Proof Dict -> case forget1 :: Proof (AlgebraType0 m (m f)) (m (m f)) of
        Proof Dict -> FreeMAlg $ ma `bindFree1` (runFreeMAlg . f)

-- |
-- Algebras for a monad @m@
class MAlg m f where
    alg :: m f a -> f a

-- newtype Nat f g a = Nat { runNat :: f a -> g a}

-- |
-- if @MAlg m a@ holds then @MAlg m (b -> a)@ holds
{--
  - algfn :: forall m f g a .
  -          ( FreeAlgebra1 m
  -          , AlgebraType0 m g
  -          , AlgebraType0 m (Nat f g)
  -          , MAlg m g
  -          )
  -       => m (Nat f g) a
  -       -> Nat f g a
  - algfn mftog = Nat $ \fa -> alg $ hoistFree1 (evalNat fa) mftog
  -     where
  -     evalNat :: forall x. f x -> Nat f g x -> g x
  -     evalNat fx (Nat nat) = nat fx
  --}

algFreeMAlg
    :: ( FreeAlgebra1 m
       , AlgebraType0 m f
       , AlgebraType0 m (m f)
       , AlgebraType0 m (FreeMAlg m f)
       )
    => m (FreeMAlg m f) a
    -> FreeMAlg m f a
algFreeMAlg ma = FreeMAlg $ joinFree1 $ hoistFree1 runFreeMAlg ma

-- |
-- Unwrapped version of @'returnF'@
returnFreeMAlg
    :: ( FreeAlgebra1 m
       , AlgebraType0 m f
       )
    => f a
    -> FreeMAlg m f a
returnFreeMAlg = FreeMAlg . liftFree

foldMapFreeMAlg
    :: ( AlgebraType0 m f
       , AlgebraType0 m d
       , MAlg m d
       )
    => (forall x. f x -> d x)
    -> (FreeMAlg m f a -> d a)
foldMapFreeMAlg fn (FreeMAlg ma) = alg $ hoistFree1 fn ma

foldFreeMAlg
    :: ( AlgebraType0 m f
       , MAlg m f
       ) 
    => FreeMAlg m f a -> f a
foldFreeMAlg = foldMapFreeMAlg id

-- |
-- The comparison functor from the category of algebras of type @AgelbraType
-- m a@ to the category of @MAlg m a@.
-- A category is monadic iff @k@ is an equivalence of categories.
-- This is true for all categories of algebras which have an @FreeAlgebra m@
-- instance.  The inverse is more interesting, since it constructs an instance
-- @AlgebraType m a@ on @a@ out of @m a -> a@.  Some examples are given below.
k :: ( FreeAlgebra1 m
     , AlgebraType  m f
     )
  => Proxy a
  -> (m f a -> f a)
k _ = foldFree1
