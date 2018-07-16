{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Algebra.Free
    ( AlgebraType1
    , FreeAlgebra1 (..)
    , foldFree1
    , hoistFree1
    , hoistHH
    , fmapFree1
    , joinFree1
    , bindFree1
    , assocFree1
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

import           Data.Algebra.Free (AlgebraType)

-- |
-- The constraint that will be imposed on the generator type of kind (* -> *).
type family AlgebraType1 (m :: k) (b :: * -> *) :: Constraint

-- |
-- Higher kinded version of @'FreeAlgebra'@.  Instances includes free functors,
-- free applicative functors and free monads.
class FreeAlgebra1 (m :: (* -> *) -> * -> *) where
    -- | Natural transformation that embeds generators into @m@.
    returnFree1 :: AlgebraType1 m f => f a -> m f a

    -- | The freeness property.  It is generalised to allow change of
    -- coefficients with a map (@a -> b@).
    foldMapFree1
        :: forall (d :: * -> *) f a b .
           ( AlgebraType m d
           , AlgebraType1 m f
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
foldFree1 :: ( FreeAlgebra1 m
             , AlgebraType m f
             , AlgebraType1 m f
             )
          => m f a
          -> f a
foldFree1 = foldMapFree1 id id

-- |
-- This is Functor instance for @m@ when considered as en endofuctor of some
-- subcategory of @* -> *@ (e.g. endofunctors of _Hask_).
--
-- It can be specialized to:
--
-- * @'Control.Applicative.Free.hoistAp'@
-- * @'Control.Monad.Free.hoistFree'@
hoistFree1 :: forall m f g a .
              ( FreeAlgebra1 m
              , AlgebraType m (m g)
              , AlgebraType1 m f
              , AlgebraType1 m g
              )
           => (forall x. f x -> g x) -- ^ a natural transformation @f ~> g@
           -> m f a
           -> m g a
hoistFree1 nat = foldMapFree1 (returnFree1 . nat) id

-- |
-- prop> hoistHH . hoistHH = hoistHH
hoistHH :: forall m n f a .
           ( AlgebraType m (n f)
           , AlgebraType1 m f
           , AlgebraType1 n f
           , FreeAlgebra1 m
           , FreeAlgebra1 n
           )
        => m f a
        -> n f a
hoistHH = foldMapFree1 returnFree1 id

-- |
-- @'FreeAlgebraH' m@ implies that @m f@ is a functor.
fmapFree1 :: forall m f a b .
         ( FreeAlgebra1 m
         , AlgebraType m (m f)
         , AlgebraType1 m f
         )
      => (a -> b)
      -> m f a
      -> m f b
fmapFree1 f mfa = foldMapFree1 returnFree1 f mfa

-- |
-- @'joinH'@ makes @m@ a monad in some subcatgory of types of kind @* -> *@
-- (usually the end-functor category of @Hask@).  It is just a specialization
-- of @'foldH'@.
joinFree1 :: forall m f a .
             ( FreeAlgebra1 m
             , AlgebraType m (m f)
             , AlgebraType1 m (m f)
             )
          => m (m f) a
          -> m f a
joinFree1 = foldFree1

-- |
-- Bind operator for the @'joinH'@ monad
bindFree1 :: forall m f g a b .
             ( FreeAlgebra1 m
             , AlgebraType m (m g)
             , AlgebraType m (m (m g))
             , AlgebraType1 m (m g)
             , AlgebraType1 m f
             )
          => m f a
          -> (forall x . f x -> m g x) -- ^ natural transformation @f ~> m g@
          -> m g a
bindFree1 mfa nat = joinFree1 $ hoistFree1 nat mfa

assocFree1 :: forall m f a .
              ( FreeAlgebra1 m
              , AlgebraType m f
              , AlgebraType m (m (m f))
              , AlgebraType1 m f
              , AlgebraType1 m (m f)
              )
           => m f (m f a)
           -> m (m f) (f a)
assocFree1 = foldMapFree1 f g
    where
        f :: forall x. f x -> m (m f) x
        f = hoistFree1 returnFree1 . returnFree1

        g :: m f a -> f a
        g = foldFree1

type instance AlgebraType  Coyoneda g = Functor g
type instance AlgebraType1 Coyoneda g = ()
instance FreeAlgebra1 Coyoneda where
    returnFree1 :: f a -> Coyoneda f a
    returnFree1 = liftCoyoneda

    foldMapFree1 :: Functor g
                 => (forall x. f x -> g x)
                 -> (a -> b)
                 -> Coyoneda f a
                 -> g b
    foldMapFree1 nat f (Coyoneda ba fx) = fmap f ba <$> nat fx

type instance AlgebraType  Ap g = Applicative g
type instance AlgebraType1 Ap g = Functor g
instance FreeAlgebra1 Ap where
    returnFree1 :: Functor f => f a -> Ap f a
    returnFree1 = Ap.liftAp

    foldMapFree1 :: forall (d :: * -> *) f a b .
                    ( Functor f
                    , Applicative d
                    )
                 => (forall x. f x -> d x)
                 -> (a -> b)
                 -> (Ap f a -> d b)
    foldMapFree1 _   f (Ap.Pure a) = pure $ f a
    foldMapFree1 nat f (Ap.Ap fx apxa)
        = fmap f $ foldMapFree1 nat id apxa <*> nat fx

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
type instance AlgebraType1 DayF g = Functor g

instance FreeAlgebra1 DayF where
    returnFree1 :: Functor f => f a -> DayF f a
    returnFree1 fa = DayF $ Day fa fa const

    foldMapFree1 :: ( Functor f
                    , Applicative d
                    )
                 => (forall x. f x -> d x)
                 -> (a -> b)
                 -> DayF f a
                 -> d b
    foldMapFree1 nat f (DayF day)
        = fmap f $ Day.dap . Day.trans2 nat . Day.trans1 nat $ day

type instance AlgebraType  Free m = Monad m
type instance AlgebraType1 Free f = Functor f

instance FreeAlgebra1 Free where
    returnFree1 :: Functor f => f a -> Free f a
    returnFree1 = Free.liftF

    foldMapFree1 :: (Functor f, Monad d)
                 => (forall x. f x -> d x)
                 -> (a -> b)
                 -> Free f a
                 -> d b
    foldMapFree1 nat f ff = f <$> Free.foldFree nat ff
