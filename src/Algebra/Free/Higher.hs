{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Algebra.Free.Higher
    ( AlgebraTypeH
    , FreeAlgebraH (..)
    , foldH
    , hoistH
    , hoistHH
    , fmapH
    , joinH
    , bindH
    , assocH
    , DayF (..)
    , dayToAp
    , apToDay
    ) where

import           Control.Applicative.Free (Ap)
import qualified Control.Applicative.Free as Ap
import           Control.Monad.Free (Free)
import qualified Control.Monad.Free as Free
import           Data.Kind (Constraint)
import           Data.Functor.Day (Day (..))
import qualified Data.Functor.Day as Day
import           Data.Functor.Coyoneda (Coyoneda (..), liftCoyoneda)

import           Algebra.Free.Class (AlgebraType)

-- |
-- The constraint that will be imposed on the generator type of kind (* -> *).
type family AlgebraTypeH (m :: k) (b :: * -> *) :: Constraint

-- |
-- Higher kinded version of @'FreeAlgebra'@.  Instances includes free functors,
-- free applicative functors and free monads.
class FreeAlgebraH (m :: (* -> *) -> * -> *) where
    -- | Natural transformation that embeds generators into @m@.
    returnH :: AlgebraTypeH m f => f a -> m f a

    -- | The freeness property.  It is generalised to allow change of
    -- coefficients with a map (@a -> b@).
    foldMapH
        :: forall (d :: * -> *) f a b .
           ( AlgebraType m d
           , AlgebraTypeH m f
           )
        => (forall x. f x -> d x)
        -- ^ natural transformation which embeds generators of @m@ into @d@
        -> (a -> b)
        -- ^ change of coefficients
        -> (m f a -> d b)
        -- ^ a homomorphism from @m@ to @d@

-- |
-- @'FreeAlgebraH' m@ implies that @m f@ is a foldable:
-- It can be specialized to:
--
-- * @'Data.Functor.Contravariant.Coyoneda.lowerCoyoneda'@
-- * @'Control.Applicative.Free.retractAp'@
-- * @'Control.Monad.Free.foldFree'@
foldH :: ( FreeAlgebraH m
         , AlgebraType m f
         , AlgebraTypeH m f
         )
      => m f a
      -> f a
foldH = foldMapH id id

-- |
-- This is Functor instance for @m@ when considered as en endofuctor of some
-- subcategory of @* -> *@ (e.g. endofunctors of _Hask_).
--
-- It can be specialized to:
--
-- * @'Control.Applicative.Free.hoistAp'@
-- * @'Control.Monad.Free.hoistFree'@
hoistH :: forall m f g a .
          ( FreeAlgebraH m
          , AlgebraType m (m g)
          , AlgebraTypeH m f
          , AlgebraTypeH m g
          )
       => (forall x. f x -> g x) -- ^ a natural transformation @f ~> g@
       -> m f a
       -> m g a
hoistH nat = foldMapH (returnH . nat) id

-- |
-- prop> hoistHH . hoistHH = hoistHH
hoistHH :: forall m n f a .
           ( AlgebraType m (n f)
           , AlgebraTypeH m f
           , AlgebraTypeH n f
           , FreeAlgebraH m
           , FreeAlgebraH n
           )
        => m f a
        -> n f a
hoistHH = foldMapH returnH id

-- |
-- @'FreeAlgebraH' m@ implies that @m f@ is a functor.
fmapH :: forall m f a b .
         ( FreeAlgebraH m
         , AlgebraType m (m f)
         , AlgebraTypeH m f
         )
      => (a -> b)
      -> m f a
      -> m f b
fmapH f mfa = foldMapH returnH f mfa

-- |
-- @'joinH'@ makes @m@ a monad in some subcatgory of types of kind @* -> *@
-- (usually the end-functor category of @Hask@).  It is just a specialization
-- of @'foldH'@.
joinH :: forall m f a .
         ( FreeAlgebraH m
         , AlgebraType m (m f)
         , AlgebraTypeH m (m f)
         )
      => m (m f) a
      -> m f a
joinH = foldH

-- |
-- Bind operator for the @'joinH'@ monad
bindH :: forall m f g a b .
         ( FreeAlgebraH m
         , AlgebraType m (m g)
         , AlgebraType m (m (m g))
         , AlgebraTypeH m (m g)
         , AlgebraTypeH m f
         )
      => m f a
      -> (forall x . f x -> m g x) -- ^ natural transformation @f ~> m g@
      -> m g a
bindH mfa nat = joinH $ hoistH nat mfa

assocH :: forall m f a .
          ( FreeAlgebraH m
          , AlgebraType m f
          , AlgebraType m (m (m f))
          , AlgebraTypeH m f
          , AlgebraTypeH m (m f)
          )
       => m f (m f a)
       -> m (m f) (f a)
assocH = foldMapH f g
    where
        f :: forall x. f x -> m (m f) x
        f = hoistH returnH . returnH

        g :: m f a -> f a
        g = foldH

type instance AlgebraType  Coyoneda g = Functor g
type instance AlgebraTypeH Coyoneda g = ()
instance FreeAlgebraH Coyoneda where
    returnH :: f a -> Coyoneda f a
    returnH = liftCoyoneda

    foldMapH :: Functor g
             => (forall x. f x -> g x)
             -> (a -> b)
             -> Coyoneda f a
             -> g b
    foldMapH nat f (Coyoneda ba fx) = fmap f ba <$> nat fx

type instance AlgebraType  Ap g = Applicative g
type instance AlgebraTypeH Ap g = Functor g
instance FreeAlgebraH Ap where
    returnH :: Functor f => f a -> Ap f a
    returnH = Ap.liftAp

    foldMapH :: forall (d :: * -> *) f a b .
                ( Functor f
                , Applicative d
                )
             => (forall x. f x -> d x)
             -> (a -> b)
             -> (Ap f a -> d b)
    foldMapH _   f (Ap.Pure a) = pure $ f a
    foldMapH nat f (Ap.Ap fx apxa)
        = fmap f $ foldMapH nat id apxa <*> nat fx

-- |
-- @Day f f@ newtype wrapper.  It is isomorphic with @Ap f@ for applicative
-- functors @f@ via @'dayToAp'@ (and @'dayToAp'@).
newtype DayF f a = DayF { runDayF :: Day f f a}
    deriving (Functor, Applicative)

dayToAp :: Applicative f => Day f f a -> Ap f a
dayToAp =  hoistHH . DayF

apToDay :: Applicative f => Ap f a -> Day f f a
apToDay = runDayF . hoistHH

type instance AlgebraType  DayF g = Applicative g
type instance AlgebraTypeH DayF g = Functor g

instance FreeAlgebraH DayF where
    returnH :: Functor f => f a -> DayF f a
    returnH fa = DayF $ Day fa fa const

    foldMapH :: ( Functor f
                , Applicative d
                )
                => (forall x. f x -> d x)
                -> (a -> b)
                -> DayF f a
                -> d b
    foldMapH nat f (DayF day)
        = fmap f $ Day.dap . Day.trans2 nat . Day.trans1 nat $ day

type instance AlgebraType  Free m = Monad m
type instance AlgebraTypeH Free f = Functor f

instance FreeAlgebraH Free where
    returnH :: Functor f => f a -> Free f a
    returnH = Free.liftF

    foldMapH :: (Functor f, Monad d)
             => (forall x. f x -> d x)
             -> (a -> b)
             -> Free f a
             -> d b
    foldMapH nat f ff = f <$> Free.foldFree nat ff
