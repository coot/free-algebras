{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Algebra.Free
    ( -- * Free algebra class
      FreeAlgebra (..)
      -- ** Type level witnesses
    , Proof (..)
    , -- ** Algebra types \/ constraints
      AlgebraType
    , AlgebraType0
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
      -- * General free type
    , Free (..)
    , DNonEmpty (..)
    )
    where

import           Prelude

import           Data.DList as DList
import           Data.Functor.Identity (Identity (..))
#if MIN_VERSION_data_fix(0,3,0)
import           Data.Fix (Fix, foldFix)
#else
import           Data.Fix (Fix, cata)
#endif
import           Data.Group (Group (..))
import           Data.Kind (Constraint, Type)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid (Endo (..), Dual (..))
import           Data.Algebra.Pointed (Pointed (..))

--
-- Prerequisites for @'FreeAlgebra'@
--

-- | Type family which for each free algebra @m@ returns a type level lambda
-- from types to constraints.  It is describe the class of algebras for which
-- this free algebra is free.
--
-- A lawful instance for this type family must guarantee
-- that the constraint @'AlgebraType0' m f@ is implied by the @'AlgebraType'
-- m f@ constraint.  This guarantees that there exists a forgetful functor from
-- the category of types of kind @* -> *@ which satisfy @'AlgebraType' m@
-- constrain to the category of types of kind @* -> *@ which satisfy the
-- @'AlgebraType0 m@ constraint.
--
type family AlgebraType  (f :: k) (a :: l) :: Constraint

-- | Type family which limits Hask to its full subcategory which satisfies
-- a given constraints.  Some free algebras, like free groups, or free abelian
-- semigroups have additional constraints on on generators, like @Eq@ or @Ord@.
--
type family AlgebraType0 (f :: k) (a :: l) :: Constraint

-- | A proof that constraint @c@ holds for type @a@.
--
data Proof (c :: Constraint) (a :: l) where
    Proof :: c => Proof c a

-- | A lawful instance has to guarantee that @'unFoldFree'@ is an inverse of
-- @'foldMapFree'@ (in the category of algebras of type @'AlgebraType' m@).
--
-- This in turn guaranties that @m@ is a left adjoint functor from full
-- subcategory of Hask (of types constrained by @'AlgebraType0' m) to algebras
-- of type @'AlgebraType' m@.  The right adjoint is the forgetful functor.  The
-- composition of left adjoin and the right one is always a monad, this is why
-- we will be able to build monad instance for @m@.
--
class FreeAlgebra (m :: Type -> Type)  where

    {-# MINIMAL returnFree, foldMapFree #-}

    -- | Injective map that embeds generators @a@ into @m@.
    returnFree :: a -> m a

    -- | The freeness property.
    foldMapFree
        :: forall d a
         . ( AlgebraType m d
           , AlgebraType0 m a
           )
        => (a -> d)   -- ^ a mapping of generators of @m@ into @d@
        -> (m a -> d) -- ^ a homomorphism from @m a@ to @d@

    -- | Proof that @AlgebraType0 m a => m a@ is an algebra of type
    -- @AlgebraType m@.  This proves that @m@ is a mapping from the full
    -- subcategory of @Hask@ of types satisfying @AlgebraType0 m a@ constraint
    -- to the full subcategory satisfying @AlgebraType m a@, @'fmapFree'@ below
    -- proves that it's a functor.  (@'codom'@ from codomain)
    --
    codom  :: forall a. AlgebraType0 m a => Proof (AlgebraType m (m a)) (m a)

    default codom :: forall a. AlgebraType m (m a)
                  => Proof (AlgebraType m (m a)) (m a)
    codom = Proof

    -- | Proof that the forgetful functor from types @a@ satisfying
    -- @AgelbraType m a@ to @AlgebraType0 m a@ is well defined.
    --
    forget :: forall a. AlgebraType  m a => Proof (AlgebraType0 m a) (m a)

    default forget :: forall a. AlgebraType0 m a
                   => Proof (AlgebraType0 m a) (m a)
    forget = Proof

--
-- Free combinators
--

-- | Inverse of @'foldMapFree'@
--
-- It is uniquely determined by its universal property (by Yoneda lemma):
--
-- prop> unFoldMapFree id = returnFree
--
-- Note that @'unFoldMapFree' id@ is the unit of the
-- [unit](https://ncatlab.org/nlab/show/unit+of+an+adjunction) of the
-- adjunction imposed by the @'FreeAlgebra'@ constraint.
--
unFoldMapFree
    :: FreeAlgebra m
    => (m a -> d)
    -> (a -> d)
unFoldMapFree f = f . returnFree
{-# INLINABLE unFoldMapFree #-}

-- | All types which satisfy @'FreeAlgebra'@ constraint are foldable.
--
-- prop> foldFree . returnFree == id
--
-- @foldFree@ is the
-- [unit](https://ncatlab.org/nlab/show/unit+of+an+adjunction) of the
-- adjunction imposed by @FreeAlgebra@ constraint.
--
-- Examples:
--
-- > foldFree @[] = foldMap id
-- >              = foldr (<>) mempty
-- > foldFree @NonEmpty
-- >              = foldr1 (<>)
--
-- Note that @foldFree@ replaces the abstract \/ free algebraic operation in
-- @m a@ to concrete one in @a@.
--
foldFree
    :: forall m a .
       ( FreeAlgebra  m
       , AlgebraType  m a
       )
    => m a
    -> a
foldFree ma = case forget @m @a of
    Proof -> foldMapFree id ma
{-# INLINABLE foldFree #-}

-- | The canonical quotient map from a free algebra of a wider class to a free
-- algebra of a narrower class, e.g. from a free semigroup to
-- free monoid, or from a free monoid to free commutative monoid,
-- etc.
--
-- prop> natFree . natFree == natFree
-- prop> fmapFree f . natFree == hoistFree . fmapFree f
--
-- the constraints:
-- * the algebra @n a@ is of the same type as algebra @m@ (this is
--    always true, just GHC cannot prove it here)
-- * @m@ is a free algebra generated by @a@
-- * @n@ is a free algebra generated by @a@
--
natFree :: forall m n a .
           ( FreeAlgebra  m
           , FreeAlgebra  n
           , AlgebraType0 m a
           , AlgebraType  m (n a)
           )
        => m a
        -> n a
natFree = foldMapFree returnFree
{-# INLINABLE natFree #-}

-- | All types which satisfy @'FreeAlgebra'@ constraint are functors.  The
-- constraint @'AlgebraType' m (m b)@ is always satisfied.
--
fmapFree :: forall m a b .
            ( FreeAlgebra  m
            , AlgebraType0 m a
            , AlgebraType0 m b
            )
         => (a -> b)
         -> m a
         -> m b
fmapFree f ma = case codom @m @b of
    Proof -> foldMapFree (returnFree . f) ma
{-# INLINABLE fmapFree #-}

-- | @'FreeAlgebra'@ constraint implies @Monad@ constrain.
--
joinFree :: forall m a .
          ( FreeAlgebra  m
          , AlgebraType0 m a
          )
         => m (m a)
         -> m a
joinFree mma = case codom @m @a of
    Proof -> foldFree mma
{-# INLINABLE joinFree #-}

-- | The monadic @'bind'@ operator.  @'returnFree'@ is the corresponding
-- @'return'@ for this monad.  This just @'foldMapFree'@ in disguise.
--
bindFree :: forall m a b .
            ( FreeAlgebra  m
            , AlgebraType0 m a
            , AlgebraType0 m b
            )
         => m a
         -> (a -> m b)
         -> m b
bindFree ma f = case codom @m @b of
    Proof -> foldMapFree f ma
{-# INLINABLE bindFree #-}

-- | @'Fix' m@ is the initial algebra in the category of algebras of type
-- @'AlgebraType' m@ (the initial algebra is a free algebra generated by empty
-- set of generators, e.g. the @Void@ type).
--
-- Another way of putting this is observing that @'Fix' m@ is isomorphic to @m
-- Void@ where @m@ is the /free algebra/.  This isomorphisms is given by
-- @
--   fixToFree :: (FreeAlgebra m, AlgebraType m (m Void), Functor m) => Fix m -> m Void
--   fixToFree = cataFree
-- @
-- For monoids the inverse is given by @'Data.Fix.ana' (\_ -> [])@.
--
cataFree :: ( FreeAlgebra  m
            , AlgebraType  m a
            , Functor m
            )
         => Fix m
         -> a
#if MIN_VERSION_data_fix(0,3,0)
cataFree = foldFix foldFree
#else
cataFree = cata foldFree
#endif

-- | A version of @'Data.Foldable.foldr'@, e.g. it can specialize to
--
-- * @foldrFree \@[] :: (a -> b -> b) -> [a] -> b -> b@
-- * @foldrFree \@'Data.List.NonEmpty.NonEmpty' :: (a -> b -> b) -> 'Data.List.NonEmpty.NonEmpty' a -> b -> b@
--
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

-- | Like @'foldrFree'@ but strict.
--
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

-- | Generalizes @'Data.Foldable.foldl'@, e.g. it can specialize to
--
-- * @foldlFree \@[] :: (b -> a -> b) -> b -> [a] -> b@
-- * @foldlFree \@'Data.List.NonEmpty.NonEmpty' :: (b -> a -> b) -> b -> 'Data.List.NonEmpty.NonEmpty' a -> b@
--
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

-- | Like @'foldlFree'@ but strict.
--
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


--
-- Instances
--


type instance AlgebraType0 Identity a = ()
type instance AlgebraType  Identity a = ()
instance FreeAlgebra Identity where
    returnFree = Identity
    foldMapFree f = f . runIdentity

type instance AlgebraType0 NonEmpty a = ()
type instance AlgebraType  NonEmpty m = Semigroup m
-- | @'NonEmpty'@ is the free semigroup in the class of semigroup which are
-- strict in the left argument.
--
instance FreeAlgebra NonEmpty where
    returnFree a = a :| []
    -- @'foldMap'@ requires @'Monoid' d@ constraint which we don't need to
    -- satisfy here
    foldMapFree f (a :| []) = f a
    foldMapFree f (a :| (b : bs)) = f a <> foldMapFree f (b :| bs)

-- | 'DNonEmpty' is the free semigroup in the class of all semigroups.
--
newtype DNonEmpty a = DNonEmpty ([a] -> NonEmpty a)
instance Semigroup (DNonEmpty a) where
    DNonEmpty f <> DNonEmpty g = DNonEmpty (f . NonEmpty.toList . g)

type instance AlgebraType0 DNonEmpty a = ()
type instance AlgebraType  DNonEmpty m = Semigroup m
instance FreeAlgebra DNonEmpty where
    returnFree a = DNonEmpty (a :|)
    foldMapFree f (DNonEmpty g) = foldMapFree f (g [])

type instance AlgebraType0 [] a = ()
type instance AlgebraType  [] m = Monoid m
-- | Note that @'[]'@ is a free monoid only for monoids which multiplication is
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
--
instance FreeAlgebra [] where
    returnFree a = [a]
    foldMapFree = foldMap

type instance AlgebraType0 Maybe a = ()
type instance AlgebraType  Maybe m = Pointed m
instance FreeAlgebra Maybe where
    returnFree = Just
    foldMapFree _ Nothing  = point
    foldMapFree f (Just a) = f a

-- | @'Free' c a@ represents free algebra for a constraint @c@ generated by
-- type @a@.
--
newtype Free (c :: Type -> Constraint) a = Free {
          runFree :: forall r. c r => (a -> r) -> r
        }

instance Semigroup (Free Semigroup a) where
    Free f <> Free g = Free $ \k -> f k <> g k

type instance AlgebraType0 (Free Semigroup) a = ()
type instance AlgebraType  (Free Semigroup) a = Semigroup a
instance FreeAlgebra (Free Semigroup) where
    returnFree a = Free $ \k -> k a
    foldMapFree f (Free k) = k f

instance Semigroup (Free Monoid a) where
    Free f <> Free g = Free $ \k -> f k `mappend` g k

instance Monoid (Free Monoid a) where
    mempty = Free (const mempty)
#if __GLASGOW_HASKELL__ <= 802
    mappend = (<>)
#endif


type instance AlgebraType0 (Free Monoid) a = ()
type instance AlgebraType  (Free Monoid) a = Monoid a
instance FreeAlgebra (Free Monoid) where
    returnFree a = Free $ \k -> k a
    foldMapFree f (Free k) = k f

type instance AlgebraType0 DList a = ()
type instance AlgebraType  DList a = Monoid a
-- | @'DList'@ is isomorphic to @'Free' Monoid@; it is free in the class of all
-- monoids.
--
instance FreeAlgebra DList where
    returnFree = DList.singleton
    foldMapFree = foldMap

instance Semigroup (Free Group a) where
    Free f <> Free g = Free $ \k -> f k `mappend` g k

instance Monoid (Free Group a) where
    mempty = Free (const mempty)
#if __GLASGOW_HASKELL__ <= 802
    mappend = (<>)
#endif

instance Group (Free Group a) where
    invert (Free k) = Free (k . invert)

type instance AlgebraType0 (Free Group) a = ()
type instance AlgebraType  (Free Group) a = Group a
instance FreeAlgebra (Free Group) where
    returnFree a = Free $ \k -> k a
    foldMapFree f (Free k) = k f
