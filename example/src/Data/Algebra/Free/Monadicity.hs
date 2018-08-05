{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
module Data.Algebra.Free.Monadicity where

import           Prelude

import           Control.Monad (join)
import           Data.Constraint (Dict (..))
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Proxy (Proxy (..))

import           Data.Group.Free (FreeGroup)
import qualified Data.Group.Free as FreeGroup
import           Data.Monoid.MSet (FreeMSet (..))
import           Data.Algebra.Free

-- |
-- Full subcategory of @Hask@.
data Hom m a b where
    Hom :: (AlgebraType0 m a, AlgebraType0 m b) => (a -> b) -> Hom m a b

unHom :: Hom m a b -> a -> b
unHom (Hom f) = f

-- |
-- @'Hom'@ is a category.
composeHom :: Hom m b c -> Hom m a b -> Hom m a c
composeHom (Hom f) (Hom g) = Hom (f . g)

idHom :: AlgebraType0 m a => Hom m a a
idHom = Hom id

bimapHom :: forall m a' a b b'.
            ( AlgebraType0 m a'
            , AlgebraType0 m b'
            )
         => (a' -> a)
         -> (b  -> b')
         -> Hom m a b
         -> Hom m a' b'
bimapHom f g (Hom ab) = Hom (g . ab . f)

-- |
-- Category of algebras @a@ which fulfil the constraint @AlgebraType m a@.
data AlgHom m a b where
    AlgHom :: ( AlgebraType0 m a
              , AlgebraType0 m b
              , AlgebraType  m a
              , AlgebraType  m b
              )
           => (a -> b)
           -> AlgHom m a b

unAlgHom :: AlgHom m a b -> a -> b
unAlgHom (AlgHom f) = f

forget :: AlgHom m a b -> Hom m a b
forget (AlgHom f) = Hom f

-- |
-- @'AlgHom'@ is a category
composeAlgHom :: AlgHom m b c -> AlgHom m a b -> AlgHom m a c
composeAlgHom (AlgHom f) (AlgHom g) = AlgHom (f . g)

idAlgHom :: (AlgebraType0 m a, AlgebraType m a) => AlgHom m a a
idAlgHom = AlgHom id

bimapAlgHom :: forall m a' a b b'.
               ( AlgebraType0 m a'
               , AlgebraType0 m b'
               , AlgebraType  m a'
               , AlgebraType  m b'
               )
            => (a' -> a)
            -> (b  -> b')
            -> AlgHom m a b
            -> AlgHom m a' b'
bimapAlgHom f g (AlgHom ab) = AlgHom (g . ab . f)

-- |
-- Isomorphism of @AlgHom m (m a) d@ and @Hom m a d@ with inverse @'left'@.
right :: forall m a d .
         ( FreeAlgebra  m
         , AlgebraType0 m d -- it should be implied by `AlgebraType m d` 
                            -- (possibly by adding a proof in `FreeAlgebra`
                            -- type class)
         , AlgebraType0 m a
         )
      => AlgHom m (m a) d
      -> Hom m a d
right (AlgHom f) = case proof :: Proof (AlgebraType m (m a)) (m a) of
    Proof Dict -> Hom $ unFoldMapFree f

-- |
-- Inverse of @'right'@
left :: forall m a d .
        ( FreeAlgebra  m
        , AlgebraType  m d
        , AlgebraType0 m a
        , AlgebraType0 m (m a) -- this should be implied (possibly by adding
                               -- another proof in FreeAlgebra type class)
        )
     => Hom m a d
     -> AlgHom m (m a) d
left (Hom f) = case proof :: Proof (AlgebraType m (m a)) (m a) of
    Proof Dict -> AlgHom $ foldMapFree f

-- |
-- unit of the adjunction
unit :: forall m a .
        ( FreeAlgebra  m
        , AlgebraType0 m a
        , AlgebraType0 m (m a) -- it should be implies by `AlgebraType0 m a`
        )
     => Hom m a (m a)
unit = case proof :: Proof (AlgebraType m (m a)) (m a) of
    Proof Dict -> right (AlgHom id)

-- |
-- counit of the adjunction
counit :: forall m d .
          ( FreeAlgebra  m
          , AlgebraType  m d
          , AlgebraType0 m d -- it should be implied by `AlgebraType m d`
          , AlgebraType0 m (m d)
          )
       => AlgHom m (m d) d
counit = left (Hom id)

-- |
-- The monad associated with the adjunction.  Note that it's isomorphic to @m@
data FreeMAlg (m :: * -> *) (a :: *) where
    FreeMAlg :: FreeAlgebra m => m a -> FreeMAlg m a

runFreeMAlg :: FreeMAlg m a -> m a
runFreeMAlg (FreeMAlg ma) = ma

-- |
-- unit of the @FreeMAlg@ monad (i.e. @return@ in Haskell)
returnF :: forall m a .
           ( FreeAlgebra  m
           , AlgebraType0 m a
           , AlgebraType0 m (m a)
           , AlgebraType0 m (FreeMAlg m a)
           )
        => Hom m a (FreeMAlg m a)
returnF = case unit :: Hom m a (m a) of Hom f -> Hom (FreeMAlg . f)

-- |
-- join of the @FreeMAlg@ monad
joinF :: forall  m a .
         ( FreeAlgebra  m
         , AlgebraType0 m a
         , AlgebraType0 m (m a)
         , AlgebraType0 m (FreeMAlg m a)
         , AlgebraType0 m (FreeMAlg m (FreeMAlg m a))
         )
      => Hom m (FreeMAlg m (FreeMAlg m a)) (FreeMAlg m a)
joinF = Hom $ \(FreeMAlg mma) -> FreeMAlg $ joinFree $ fmapFree runFreeMAlg mma

-- |
-- bind of the @FreeMAlg@ monad
bindF :: forall m a b .
         ( FreeAlgebra  m
         , AlgebraType0 m b
         , AlgebraType0 m (m b)
         )
      => FreeMAlg m a
      -> Hom m a (FreeMAlg m b)
      -> FreeMAlg m b
bindF (FreeMAlg ma) (Hom f) = FreeMAlg $ ma `bindFree` (runFreeMAlg . f)

-- |
-- Algebras for a monad @m@
class MAlg m a where
    alg :: m a -> a

instance (Monad m, FreeAlgebra m) => MAlg m (FreeMAlg m a) where
    alg ma = FreeMAlg $ join $ fmap runFreeMAlg ma

-- |
-- @FreeAlg@ is an instance of @FreeAlgebra@ with @returnFreeAlg@ and
-- @foldMapFreeMalg@.  The only problem with typing this instance is that ghc
-- will not be able to deduce @AlgebraType0 m a@ instance even if we define
-- @
--    type instance AlgebraType0 (FreeAlg m) a = AlgebraType0 m a
-- @
data FreeAlg m a where
    FreeAlg :: ( FreeAlgebra m, AlgebraType0 m a ) => m a -> FreeAlg m a

runFreeAlg :: FreeAlg m a -> m a
runFreeAlg (FreeAlg ma) = ma

{--
  - instance ( FreeAlgebra m
  -          , AlgebraType0 m a
  -          , AlgebraType0 m (m a)
  -          , AlgebraType0 m (FreeAlg m a)
  -          )
  -          => MAlg m (FreeAlg m a) where
  -     alg mma = FreeAlg $ joinFree $ fmapFree runFreeAlg mma
  --}

returnFreeAlg
    :: ( FreeAlgebra m
       , AlgebraType0 m a
       )
    => a
    -> FreeAlg m a
returnFreeAlg = FreeAlg . returnFree

foldMapFreeAlg
    :: ( AlgebraType0 m a
       , AlgebraType0 m d
       , MAlg m d
       )
    => (a -> d)
    -> (FreeAlg m a -> d)
foldMapFreeAlg fn (FreeAlg ma) = alg $ fmapFree fn ma

foldFreeAlg
    :: ( AlgebraType0 m a
       , MAlg m a
       ) 
    => FreeAlg m a -> a
foldFreeAlg = foldMapFreeAlg id

-- |
-- The comparison functor from the category of algebras of type @AgelbraType
-- m a@ to the category of @MAlg m a@.
-- A category is monadic iff @k@ is an equivalence of categories.
-- This is true for all categories of algebras which have an @FreeAlgebra m@
-- instance.  The inverse is more interesting, since it constructs an instance
-- @AlgebraType m a@ on @a@ out of @m a -> a@.  Some examples are given below.
k :: ( FreeAlgebra  m
     , AlgebraType  m a
     , AlgebraType0 m a
     )
  => Proxy a
  -> (m a -> a)
k _ = foldFree

-- |
-- As above but as an instance (commented out, since its undecidable instance
-- only because the @AlgebraType m a@ is no smaller than the instance head).
{--
  - instance ( FreeAlgebra  m
  -          , AlgebraType  m a
  -          , AlgebraType0 m a
  -          )
  -          => MAlg m a where
  -     alg = k Proxy
  --}

-- |
-- @'mempty'@ deduced from @FreeAlg []@ @MAlg@ instance.
k_inv_monoid_mempty :: MAlg [] a => a
k_inv_monoid_mempty = foldFreeAlg (FreeAlg [])

-- |
-- @'mappend'@ deduced from @FreeAlg []@ @MAlg@ instance.
k_inv_monoid_mappend :: MAlg [] a => a -> a -> a
k_inv_monoid_mappend a b = foldFreeAlg (FreeAlg [a, b])

-- |
-- @'<>'@ deduced from @FreeAlg NonEmpty@ @MAlg@ instance.
k_inv_semigroup_append :: MAlg NonEmpty a => a -> a -> a
k_inv_semigroup_append a b = foldFreeAlg (FreeAlg (a :| [b]))

k_inv_pointed :: MAlg Maybe a => a
k_inv_pointed = foldFreeAlg (FreeAlg Nothing)

-- |
-- @'invert'@ deduced from @FreeAlg FreeGroup@ @MAlg@ instance.
k_inv_group_invert :: (MAlg FreeGroup a, Eq a) => a -> a
k_inv_group_invert a = foldFreeAlg (FreeAlg (FreeGroup.fromList [Left a]))

-- |
-- @'act'@ deduced from @FreeAlg FreeMSet@ @MAlg@ instance.
k_inv_act :: (MAlg (FreeMSet m) a, Monoid m) => m -> a -> a
k_inv_act m a = foldFreeAlg $ FreeAlg $ FreeMSet (m, a)

-- Some instances to play with

instance MAlg [] [a] where
    alg = concat

instance MAlg NonEmpty (NonEmpty a) where
    alg = NE.fromList . concat . fmap NE.toList . NE.toList
