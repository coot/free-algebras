{-# LANGUAGE GADTs #-}
module Data.Algebra.Free
    ( -- * Algebra type
      AlgebraType
    , AlgebraType0
      -- * FreeAlgebra class
    , FreeAlgebra (..)
    , Proof (..)
    , proof
      -- * Combinators
    , unFoldMapFree
    , foldFree
    , natFree
    , fmapFree
    , joinFree
    , bindFree
    , cataFree
    , foldrFree
    , foldrFree'
    , foldlFree
    , foldlFree'
    , Free (..)
    )
    where

import           Prelude

import           Data.Constraint (Dict (..))
import           Data.DList (DList)
import           Data.DList as DList
import           Data.Functor.Identity (Identity (..))
import           Data.Fix (Fix, cata)
import           Data.Group (Group (..))
import           Data.Kind (Constraint, Type)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Monoid (Endo (..), Monoid (..), Dual (..))
import           Data.Semigroup (Semigroup, (<>))

import           Data.Algebra.Pointed (Pointed (..))

-- |
-- Type family which for each free algebra @m@ returns a type level lambda from
-- types to constraints.  It is describe the class of algebras for which this
-- free algebra is free.
--
-- A lawful instance for this type family must guarantee
-- that the constraint @'AlgebraType0' m f@ is implied by the @'AlgebraType'
-- m f@ constraint.  This guarantees that there exists a forgetful functor from
-- the category of types of kind @* -> *@ which satisfy @'AlgebraType' m@
-- constrain to the category of types of kind @* -> *@ which satisfy the
-- @'AlgebraType0 m@ constraint.
type family AlgebraType  (f :: k) (a :: l) :: Constraint

-- |
-- Type family which limits Hask to its full subcategory which satisfies
-- a given constraints.  Some free algebras, like free groups, or free abelian
-- semigroups have additional constraints on on generators, like @Eq@ or @Ord@.
type family AlgebraType0 (f :: k) (a :: l) :: Constraint

-- |
-- A proof that constraint @c@ holds for type @a@.
newtype Proof (c :: Constraint) (a :: l) = Proof (Dict c)

-- |
-- @'Proof'@ smart constructor.
proof :: c => Proof (c :: Constraint) (a :: l)
proof = Proof Dict

-- |
-- A lawful instance has to guarantee that @'unFoldFree'@ is an inverse of
-- @'foldMapFree'@ (in the category of algebras of type @'AlgebraType' m@).
--
-- This in turn guaranties that @m@ is a left adjoint functor from full
-- subcategory of Hask (of types constrained by @'AlgebraType0' m) to algebras
-- of type @'AlgebraType' m@.  The right adjoint is the forgetful functor.  The
-- composition of left adjoin and the right one is always a monad, this is why
-- we will be able to build monad instance for @m@.
class FreeAlgebra (m :: Type -> Type)  where
    -- | Injective map that embeds generators @a@ into @m@.
    returnFree :: a -> m a
    -- | The freeness property.
    foldMapFree
        :: forall d a
         . ( AlgebraType m d
           , AlgebraType0 m a
           )
        => (a -> d)   -- ^ map generators of @m@ into @d@
        -> (m a -> d) -- ^ returns a homomorphism from @m a@ to @d@

    -- |
    -- Proof that @AlgebraType0 m a => m a@ is an algebra of type @AlgebraType m@.
    -- This proves that @m@ is a mapping from the full subcategory of @Hask@ of
    -- types satisfying @AlgebraType0 m a@ constraint to the full subcategory
    -- satisfying @AlgebraType m a@, @'fmapFree'@ below proves that it's a functor.
    -- (@'codom'@ from codomain)
    codom  :: forall a. AlgebraType0 m a => Proof (AlgebraType m (m a)) (m a)
    -- |
    -- Proof that the forgetful functor from types @a@ satisfying @AgelbraType
    -- m a@ to @AlgebraType0 m a@ is well defined.
    forget :: forall a. AlgebraType  m a => Proof (AlgebraType0 m a) (m a)


-- |
-- Inverse of @'foldMapFree'@
--
-- prop> unFoldMapFree id = returnFree
--
-- Note that @'unFoldMapFree' id@ is the unit of the
-- [unit](https://ncatlab.org/nlab/show/unit+of+an+adjunction) of the
-- adjunction imposed by the @'FreeAlgebra'@ constraint.
unFoldMapFree
    :: FreeAlgebra m
    => (m a -> d)
    -> (a -> d)
unFoldMapFree f = f . returnFree

-- |
-- All types which satisfy @'FreeAlgebra'@ constraint are foldable.
--
-- prop> foldFree . returnFree == id
--
-- @foldFree@ is the
-- [unit](https://ncatlab.org/nlab/show/unit+of+an+adjunction) of the
-- adjunction imposed by @FreeAlgebra@ constraint.
foldFree
    :: forall m a .
       ( FreeAlgebra  m
       , AlgebraType  m a
       )
    => m a
    -> a
foldFree ma = case forget @m @a of
    Proof Dict -> foldMapFree id ma

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
           ( FreeAlgebra  m
           , FreeAlgebra  n
           , AlgebraType0 m a
           , AlgebraType  m (n a)
           )
        => m a
        -> n a
natFree = foldMapFree returnFree

-- |
-- All types which satisfy @'FreeAlgebra'@ constraint are functors.
-- The constraint @'AlgebraType' m (m b)@ is always satisfied.
fmapFree :: forall m a b .
            ( FreeAlgebra  m
            , AlgebraType0 m a
            , AlgebraType0 m b
            )
         => (a -> b)
         -> m a
         -> m b
fmapFree f ma = case codom @m @b of
    Proof Dict -> foldMapFree (returnFree . f) ma

-- |
-- @'FreeAlgebra'@ constraint implies @Monad@ constrain.
joinFree :: forall m a .
          ( FreeAlgebra  m
          , AlgebraType0 m a
          )
         => m (m a)
         -> m a
joinFree mma = case codom @m @a of
    Proof Dict -> foldFree mma

-- |
-- The monadic @'bind'@ operator.  @'returnFree'@ is the corresponding
-- @'return'@ for this monad.  This just @'foldMapFree'@ in disguise.
bindFree :: forall m a b .
            ( FreeAlgebra  m
            , AlgebraType0 m a
            , AlgebraType0 m b
            )
         => m a
         -> (a -> m b)
         -> m b
bindFree ma f = case codom @m @b of
    Proof Dict -> foldMapFree f ma

-- |
-- @'Fix' m@ is the initial algebra in the category of algebras of type
-- @'AlgebraType' m@, whenever it /exists/.
--
-- Another way of putting this is observing that @'Fix' m@ is isomorphic to @m
-- Void@ where @m@ is the /free algebra/.  This isomorphisms is given by
-- @
--   fixToFree :: (FreeAlgebra m, AlgebraType m (m Void), Functor m) => Fix m -> m Void
--   fixToFree = cataFree
-- @
-- For monoids the inverse is given by @'Data.Fix.ana' (\_ -> [])@.  The
-- category of semigroups, however,  does not have the initial object.
cataFree :: ( FreeAlgebra  m
            , AlgebraType  m a
            , Functor m
            )
         => Fix m
         -> a
cataFree = cata foldFree

-- |
-- A version of @'Data.Foldable.foldr'@, e.g. it can specialize to
--
-- * @foldrFree \@[] :: (a -> b -> b) -> [a] -> b -> b@
-- * @foldrFree \@'Data.List.NonEmpty.NonEmpty' :: (a -> b -> b) -> 'Data.List.NonEmpty.NonEmpty' a -> b -> b@
foldrFree
    :: forall m a b .
       ( FreeAlgebra  m
       , AlgebraType  m (Endo b)
       , AlgebraType0 m a
       )
    => (a -> b -> b)
    -> b
    -> m a
    -> b
foldrFree f z t = appEndo (foldMapFree (Endo . f) t) z

-- |
-- Like @'foldrFree'@ but strict.
foldrFree'
    :: forall m a b .
       ( FreeAlgebra  m
       , AlgebraType  m (Dual (Endo (b -> b)))
       , AlgebraType0 m a
       )
    => (a -> b -> b)
    -> m a
    -> b
    -> b
foldrFree' f xs z0 = foldlFree f' id xs z0
    where
    f' k x z = k $! f x z

-- |
-- Generalizes @'Data.Foldabale.foldl'@, e.g. it can specialize to
--
-- * @foldlFree \@[] :: (b -> a -> b) -> b -> [a] -> b@
-- * @foldlFree \@'Data.List.NonEmpty.NonEmpty' :: (b -> a -> b) -> b -> 'Data.List.NonEmpty.NonEmpty' a -> b@
foldlFree
    :: forall m a b .
       ( FreeAlgebra  m
       , AlgebraType  m (Dual (Endo b))
       , AlgebraType0 m a
       )
    => (b -> a -> b)
    -> b
    -> m a
    -> b
foldlFree f z t = appEndo (getDual (foldMapFree (Dual . Endo . flip f) t)) z

-- |
-- Like @'foldlFree'@ but strict.
foldlFree'
    :: forall m a b .
       ( FreeAlgebra  m
       , AlgebraType  m (Endo (b -> b))
       , AlgebraType0 m a
       )
    => (b -> a -> b)
    -> b
    -> m a
    -> b
foldlFree' f z0 xs = foldrFree f' id xs z0
    where
    f' x k z = k $! f z x

type instance AlgebraType0 Identity a = ()
type instance AlgebraType  Identity a = ()
instance FreeAlgebra Identity where
    returnFree = Identity
    foldMapFree f = f . runIdentity

    codom  = proof
    forget = proof

type instance AlgebraType0 NonEmpty a = ()
type instance AlgebraType  NonEmpty m = Semigroup m
-- |
-- @'NonEmpty'@ is the free semigroup in the class of semigroup which are
-- strict in the left argument.
instance FreeAlgebra NonEmpty where
    returnFree a = a :| []
    -- @'foldMap'@ requires @'Monoid' d@ constraint which we don't need to
    -- satisfy here
    foldMapFree f (a :| []) = f a
    foldMapFree f (a :| (b : bs)) = f a <> foldMapFree f (b :| bs)

    codom  = Proof Dict
    forget = Proof Dict

type instance AlgebraType0 [] a = ()
type instance AlgebraType  [] m = Monoid m
-- | 
-- Note that @'[]'@ is a free monoid only for monoids which multiplication is
-- strict in the left argument
-- [ref](http://comonad.com/reader/2015/free-monoids-in-haskell/). Note that
-- being strict adds additional equation to the monoid laws:
--
-- prop> undefined <> a = undefined
--
-- Thus, expectedly we get an equational theory for left / right / two-sided
-- strict monoids.
--
-- Snoc lists are free monoids in the class of monoids which are strict in the
-- right argument, @'Free' Monoid@ and @'DList' are free in the class of all
-- Haskell monoids.
instance FreeAlgebra [] where
    returnFree a = [a]
    foldMapFree = foldMap

    codom  = Proof Dict
    forget = Proof Dict

type instance AlgebraType0 Maybe a = ()
type instance AlgebraType  Maybe m = Pointed m
instance FreeAlgebra Maybe where
    returnFree = Just
    foldMapFree _ Nothing  = point
    foldMapFree f (Just a) = f a

    codom  = Proof Dict
    forget = Proof Dict

-- |
-- @'Free' c a@ represents free algebra for a constraint @c@ generated by
-- type @a@.
newtype Free c a = Free { runFree :: forall r. c r => (a -> r) -> r }

instance Semigroup (Free Semigroup a) where
    Free f <> Free g = Free $ \k -> f k <> g k

type instance AlgebraType0 (Free Semigroup) a = ()
type instance AlgebraType  (Free Semigroup) a = Semigroup a
instance FreeAlgebra (Free Semigroup) where
    returnFree a = Free $ \k -> k a
    foldMapFree f (Free k) = k f

    codom  = Proof Dict
    forget = Proof Dict

instance Semigroup (Free Monoid a) where
    Free f <> Free g = Free $ \k -> f k <> g k

instance Monoid (Free Monoid a) where
    mempty = Free (const mempty)

type instance AlgebraType0 (Free Monoid) a = ()
type instance AlgebraType  (Free Monoid) a = Monoid a
instance FreeAlgebra (Free Monoid) where
    returnFree a = Free $ \k -> k a
    foldMapFree f (Free k) = k f

    codom  = proof
    forget = proof

type instance AlgebraType0 DList a = ()
type instance AlgebraType  DList a = Monoid a
-- |
-- @'DList'@ is isomorphic to @'Free' Monoid@; it is free in the class of all
-- monoids.
instance FreeAlgebra DList where
    returnFree = DList.singleton
    foldMapFree = foldMap

    codom  = proof
    forget = proof

instance Semigroup (Free Group a) where
    Free f <> Free g = Free $ \k -> f k <> g k

instance Monoid (Free Group a) where
    mempty = Free (const mempty)

instance Group (Free Group a) where
    invert (Free k) = Free (k . invert)

type instance AlgebraType0 (Free Group) a = ()
type instance AlgebraType  (Free Group) a = Group a
instance FreeAlgebra (Free Group) where
    returnFree a = Free $ \k -> k a
    foldMapFree f (Free k) = k f

    codom  = proof
    forget = proof
