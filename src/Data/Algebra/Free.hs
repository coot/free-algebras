module Data.Algebra.Free
    ( -- * Algebra type
      AlgebraType
      -- * FreeAlgebra class
    , FreeAlgebra (..)
      -- * Combinators
    , unFoldMapFree
    , foldFree
    , natFree
    , fmapFree
    , joinFree
    , bindFree
    , cataFree
    )
    where

import           Prelude

import           Data.Fix (Fix, cata)
import           Data.Kind (Constraint, Type)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Monoid (Monoid (..))
import           Data.Semigroup (Semigroup, (<>))

import           Data.Algebra.Pointed (Pointed (..))

type family AlgebraType (a :: k) (b :: l) :: Constraint

-- |
-- A lawful instance has to guarantee that @'unFoldFree'@ is an inverse of
-- @'foldMapFree'@.
-- 
-- This in turn guaranties that @m@ is a left adjoint functor from Hask to
-- algebras of type @'AlgebraType m'@.  The right adjoint is the forgetful
-- functor.  The composition of left adjoin and the right one is always
-- a monad, this is why we will be able to build monad instance for @m@.
class FreeAlgebra (m :: Type -> Type)  where
    -- | Injective map that embeds generators @a@ into @m@.
    returnFree :: a -> m a
    -- | The freeness property.
    foldMapFree
        :: forall d a
         . AlgebraType m d
        => (a -> d)   -- ^ map generators of @m@ into @d@
        -> (m a -> d) -- ^ returns a homomorphism from @m a@ to @d@

-- |
-- Inverse of @'foldMapFree'@
unFoldMapFree
    :: FreeAlgebra m
    => (m a -> d)
    -> (a -> d)
unFoldMapFree f = f . returnFree

-- |
-- All types which satisfy @'FreeAlgebra'@ constraint are foldable.  You can
-- use this map to build a @'Foldable'@ instance.
--
-- prop> foldFree . returnFree == id
foldFree
    :: ( FreeAlgebra m
       , AlgebraType m a
       )
    => m a
    -> a
foldFree = foldMapFree id

-- |
-- The canonical quotient map from a free algebra of a wider class to a free
-- algebra of a narrower class, e.g. from a free semigroup to
-- free monoid, or from a free monoid to free commutative monoid,
-- etc.
--
-- prop> natFree . natFree == natFree
-- prop> fmapFree f . natFree == hoistFree . fmapFree f
--
-- the constraints:
-- * the algebra @n a@ is of the same type as algebra @m@ (this is
--    always true, just ghc cannot prove it here)
-- * @m@ is a free algebra generated by @a@
-- * @n@ is a free algebra generated by @a@
natFree :: forall m n a .
           ( AlgebraType m (n a)
           , FreeAlgebra m
           , FreeAlgebra n
           )
        => m a
        -> n a
natFree = foldMapFree returnFree

-- |
-- All types which satisfy @'FreeAlgebra'@ constraint are functors.
-- The constraint @'AlgebraType' m (m b)@ is always satisfied.
fmapFree :: forall m a b .
            ( FreeAlgebra m
            , AlgebraType m (m b)
            )
         => (a -> b)
         -> m a
         -> m b
fmapFree f ma = foldMapFree (returnFree . f) ma

-- |
-- @'FreeAlgebra'@ constraint implies @Monad@ constrain.
joinFree :: ( FreeAlgebra m
            , AlgebraType m (m a)
            )
         => m (m a)
         -> m a
joinFree = foldFree

-- |
-- The monadic @'bind'@ operator.  @'returnFree'@ is the corresponding
-- @'return'@ for this monad.
bindFree :: ( FreeAlgebra m
            , AlgebraType m (m b)
            , AlgebraType m (m (m b))
            )
         => m a
         -> (a -> m b)
         -> m b
bindFree ma f = joinFree $ fmapFree f ma

-- |
-- @'Fix' m@ is the initial algebra in the category of algebras of type
-- @'AlgebraType' m@, whenever it /exists/.
--
-- Another way of puting this is observing that @'Fix' m@ is isomorphic to @m
-- Void@ where @m@ is the /free algebra/.  This isomorphisms is given by
-- @
--   fixToFree :: (FreeAlgebra m, AlgebraType m (m Void), Functor m) => Fix m -> m Void
--   fixToFree = cataFree
-- @
-- For monoids the inverse is given by @'Data.Fix.ana' (\_ -> [])@.  The
-- category of semigroups, however,  does not have the initial object.
cataFree :: ( FreeAlgebra m
            , AlgebraType m a
            , Functor m
            )
         => Fix m
         -> a
cataFree = cata foldFree

type instance AlgebraType NonEmpty m = Semigroup m
instance FreeAlgebra NonEmpty where
    returnFree a = a :| []
    -- @'foldMap'@ requires @'Monoid' d@ constraint which we don't need to
    -- satisfy here
    foldMapFree f (a :| []) = f a
    foldMapFree f (a :| (b : bs)) = f a <> foldMapFree f (b :| bs)

type instance AlgebraType [] m = Monoid m
instance FreeAlgebra [] where
    returnFree a = [a]
    foldMapFree = foldMap

type instance AlgebraType Maybe m = Pointed m
instance FreeAlgebra Maybe where
    returnFree = Just
    foldMapFree _ Nothing  = point
    foldMapFree f (Just a) = f a
