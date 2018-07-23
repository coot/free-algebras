{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Algebra.Free
    (
    -- * Classes
      AlgebraType0
    , AlgebraType
    , FreeAlgebra1 (..)
    -- * Combinators
    , wrapFree
    , foldFree1
    , unFoldNatFree
    , hoistFree1
    , hoistFreeH
    , fmapFree1
    , joinFree1
    , bindFree1
    , assocFree1
    , iterFree1
    -- * Day convolution
    , DayF (..)
    , dayToAp
    , apToDay
    -- * Various classes (higher algebra types)
    , MonadList (..)
    , MonadMaybe (..)
    ) where

import           Control.Applicative (Alternative)
import           Control.Applicative.Free (Ap)
import qualified Control.Applicative.Free as Ap
import qualified Control.Applicative.Free.Fast as Fast
import qualified Control.Applicative.Free.Final as Final
import           Control.Alternative.Free (Alt (..))
import qualified Control.Alternative.Free as Alt
import           Control.Monad (foldM, join)
import           Control.Monad.Except (ExceptT (..), MonadError (..))
import           Control.Monad.Free (Free)
import qualified Control.Monad.Free as Free
import qualified Control.Monad.Free.Church as Church
import           Control.Monad.List (ListT (..))
import           Control.Monad.Reader (MonadReader (..), ReaderT (..))
import           Control.Monad.RWS.Class (MonadRWS)
import           Control.Monad.RWS.Lazy as L (RWST (..))
import           Control.Monad.RWS.Strict as S (RWST (..))
import           Control.Monad.State.Class (MonadState (..))
import qualified Control.Monad.State.Lazy as L (StateT (..))
import qualified Control.Monad.State.Strict as S (StateT (..))
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Monad.Writer.Class (MonadWriter (..))
import qualified Control.Monad.Writer.Lazy as L (WriterT (..))
import qualified Control.Monad.Writer.Strict as S (WriterT (..))
import           Data.Functor.Coyoneda (Coyoneda (..), liftCoyoneda)
import           Data.Functor.Day (Day (..))
import qualified Data.Functor.Day as Day
import           Data.Functor.Identity (Identity (..))
import           Data.Kind (Constraint)
import           Data.Algebra.Free (AlgebraType)

-- |
-- A lawful instance for this type family must guarantee that the constraint
-- @'AlgebraType0' m f@ is implied by the @'AlgebraType' m f@ constraint.
--
-- This guaranees that there exists a forgetful functor from the category of
-- types of kind @* -> *@ which satisfy @'AlgebraType' m@
-- constrain to the category of types of kind @* -> *@ which satisfy the
-- @'AlgebraType0 m@ constraint.
type family AlgebraType0 (m :: k) (b :: * -> *) :: Constraint

-- |
-- Higher kinded version of @'FreeAlgebra'@.  Instances includes free functors,
-- free applicative functors, free monads, state monads etc.
--
-- A lawful instance should guarantee that @'foldNatFree'@ is an isomorphism
-- with inversese @'unFoldNatFree'@.
--
-- This guaranties that @m@ is a left adjoint functor from the category of
-- types of kind @* -> *@ which satisfy @'AlgebraType0' m@ constraint, to the
-- category of types of kind @* -> *@ which satisfy the @'AlgebraType' m@
-- constraint.  This functor is left afjoin to the forgetful functor (which is
-- well defined if the laws on @'AlgebraType0'@ family are satisfied.  This in
-- turn guarantess that @m@ componsed with this forgetful functor is a monad.
-- In result we get the monadic combinators: @'liftFree'@ (@'return'@ of
-- this monad) and @'bindFree1'@ (its @'bind'@) and @'joinFree1'@ - its
-- @'join'@ operator.
class FreeAlgebra1 (m :: (* -> *) -> * -> *) where
    -- | Natural transformation that embeds generators into @m@.
    liftFree :: AlgebraType0 m f => f a -> m f a

    -- | The freeness property.
    foldNatFree
        :: forall (d :: * -> *) f a .
           ( AlgebraType m d
           , AlgebraType0 m f
           )
        => (forall x. f x -> d x)
        -- ^ natural transformation which embeds generators of @m@ into @d@
        -> (m f a -> d a)
        -- ^ a homomorphism from @m@ to @d@

-- |
-- Anything that carries @'FreeAlgebra1'@ constraint is also an instance of
-- @'Control.Monad.Free.Class.MonadFree'@, but not vice versa. You can use
-- @'wrap'@ to define the a @'Contorl.Monad.Free.Class.MonadFree'@ instance.
-- @'ContT'@ is an example of a monad which does have an  @'FreeAlgebra1'@
-- instance, but has an @'MonadFree'@ instance.
--
-- The @'Monad'@ constrain will be satisfied for many monads through the
-- @'AlgebraType m'@ constraint.
wrapFree
    :: ( FreeAlgebra1 m
       , AlgebraType0 m f
       , Monad (m f)
       )
    => f (m f a)
    -> m f a
wrapFree = join . liftFree

-- |
-- @'unFoldNatFree'@ is an inverse of @'foldNatFree'@
unFoldNatFree
    :: ( FreeAlgebra1 m
       , AlgebraType0 m f
       )
    => (forall x . m f x -> d x)
    -> f a -> d a
unFoldNatFree nat = nat . liftFree

-- |
-- @'FreeAlgebraH' m@ implies that @m f@ is a foldable.
--
-- @
--  'foldFree1' . 'liftFree' == 'id' :: f a -> f a
-- @
--
-- It can be specialized to:
--
-- * @'Data.Functor.Coyoneda.lowerCoyoneda' :: 'Functor' f => 'Coyoneda' f a -> f a@
-- * @'Control.Applicative.Free.retractAp' :: 'Applicative' f => 'Ap' f a -> f a@
-- * @'Control.Monad.Free.foldFree' :: 'Monad' m => (forall x. f x -> m x) -> 'Free' f a -> m a@
foldFree1 :: ( FreeAlgebra1 m
             , AlgebraType m f
             , AlgebraType0 m f
             )
          => m f a
          -> f a
foldFree1 = foldNatFree id

-- |
-- This is a functor instance for @m@ when considered as an endofuctor of some
-- subcategory of @* -> *@ (e.g. endofunctors of _Hask_).
--
-- It can be specialized to:
--
-- * @'Control.Applicative.Free.hoistAp' :: (forall a. f a -> g a) -> 'Ap' f b -> 'Ap' g b @
-- * @'Control.Monad.Free.hoistFree' :: 'Functor' g => (forall a. f a -> g a) -> 'Free' f b -> 'Free' g b@
hoistFree1 :: forall m f g a .
              ( FreeAlgebra1 m
              , AlgebraType m (m g)
              , AlgebraType0 m f
              , AlgebraType0 m g
              )
           => (forall x. f x -> g x) -- ^ a natural transformation @f ~> g@
           -> m f a
           -> m g a
hoistFree1 nat = foldNatFree (liftFree . nat)

-- |
-- @
--  'hoistFreeH' . 'hoistFreeH' = 'hoistFreeH'
-- @
--
-- and when @'FreeAlgebra1' m ~ 'FreeAlgebra1' n@:
--
-- @
--  'hoistFreeH' = 'id'
-- @
hoistFreeH :: forall m n f a .
           ( AlgebraType m (n f)
           , AlgebraType0 m f
           , AlgebraType0 n f
           , FreeAlgebra1 m
           , FreeAlgebra1 n
           )
        => m f a
        -> n f a
hoistFreeH = foldNatFree liftFree

-- |
-- @'FreeAlgebraH' m@ implies that @m f@ is a functor, whenever @'AlgebraType0'
-- m f@ guarantees that @f@ is a functor.
fmapFree1 :: forall m f a b .
         ( FreeAlgebra1 m
         , AlgebraType m (m f)
         , AlgebraType0 m f
         , Functor (m f)
         )
      => (a -> b)
      -> m f a
      -> m f b
fmapFree1 f mfa = f <$> foldNatFree liftFree mfa

-- |
-- @'joinFree1'@ makes @m@ a monad in some subcatgory of types of kind @* -> *@
-- (usually the end-functor category of @Hask@).  It is just a specialization
-- of @'foldFree1'@.
joinFree1 :: forall m f a .
             ( FreeAlgebra1 m
             , AlgebraType m (m f)
             , AlgebraType0 m (m f)
             )
          => m (m f) a
          -> m f a
joinFree1 = foldFree1

-- |
-- Bind operator for the @'joinFree1'@ monad
bindFree1 :: forall m f g a .
             ( FreeAlgebra1 m
             , AlgebraType m (m g)
             , AlgebraType m (m (m g))
             , AlgebraType0 m (m g)
             , AlgebraType0 m f
             )
          => m f a
          -> (forall x . f x -> m g x) -- ^ natural transformation @f ~> m g@
          -> m g a
bindFree1 mfa nat = joinFree1 $ hoistFree1 nat mfa

assocFree1 :: forall m f a .
              ( FreeAlgebra1 m
              , AlgebraType m f
              , AlgebraType m (m (m f))
              , AlgebraType0 m f
              , AlgebraType0 m (m f)
              , Functor (m (m f))
              )
           => m f (m f a)
           -> m (m f) (f a)
assocFree1 = fmap g <$> foldNatFree f
    where
        f :: forall x. f x -> m (m f) x
        f = hoistFree1 liftFree . liftFree

        g :: m f a -> f a
        g = foldFree1

-- |
-- Specialization of @'foldNatFree' \@_ \@'Identity'@, e.g.
--
-- * @\\_ -> 'runIdentity' . 'Data.Functor.Coyoneda.lowerCoyoneda'@
-- * @'Control.Applicative.Free.iterAp' :: 'Functor' g => (g a -> a) -> 'Ap' g a -> a@
-- * @'Control.Monad.Free.iter' :: 'Functor' f => (f a -> a) -> 'Free' f a -> a@
iterFree1 :: forall m f a .
             ( FreeAlgebra1 m
             , AlgebraType0 m f
             , AlgebraType m Identity
             )
          => (forall x . f x -> x)
          -> m f a
          -> a
iterFree1 f = runIdentity . foldNatFree @_ @Identity (Identity . f)

-- Instances

-- |
-- Algebras of the same type as @'Coyoneda'@ are all functors.
type instance AlgebraType  Coyoneda g = Functor g
type instance AlgebraType0 Coyoneda g = ()
instance FreeAlgebra1 Coyoneda where
    liftFree = liftCoyoneda
    foldNatFree nat (Coyoneda ba fx) = ba <$> nat fx

-- |
-- Algebras of the same type as @'Ap'@ are the applicative functors.
type instance AlgebraType  Ap g = Applicative g
type instance AlgebraType0 Ap g = Functor g
-- |
-- @'Ap'@ is a free in the class of applicative functors, over any functor
-- (@'Ap' f@ is applicative whenever @f@ is a functor)
instance FreeAlgebra1 Ap where
    liftFree  = Ap.liftAp
    foldNatFree = Ap.runAp

type instance AlgebraType  Fast.Ap g = Applicative g
type instance AlgebraType0 Fast.Ap g = Functor g
instance FreeAlgebra1 Fast.Ap where
    liftFree  = Fast.liftAp
    foldNatFree = Fast.runAp

type instance AlgebraType  Final.Ap g = Applicative g
type instance AlgebraType0 Final.Ap g = Functor g
instance FreeAlgebra1 Final.Ap where
    liftFree  = Final.liftAp
    foldNatFree = Final.runAp

-- |
-- @'Day' f f@ newtype wrapper.  It is isomorphic with @'Ap' f@ for applicative
-- functors @f@ via @'dayToAp'@ (and @'dayToAp'@).
newtype DayF f a = DayF { runDayF :: Day f f a}
    deriving (Functor, Applicative)

dayToAp :: Applicative f => Day f f a -> Ap f a
dayToAp =  hoistFreeH . DayF

apToDay :: Applicative f => Ap f a -> Day f f a
apToDay = runDayF . hoistFreeH

-- |
-- Algebras of the same type as @'DayF'@ are all the applicative functors.
type instance AlgebraType  DayF g = Applicative g
type instance AlgebraType0 DayF g = Functor g
-- |
-- @'DayF'@, as @'Ap'@ is a free applicative functor, but over applicative functors
-- (@'DayF' f@ is applicative if @f@ is an applicative functor).
instance FreeAlgebra1 DayF where
    liftFree fa = DayF $ Day fa fa const
    foldNatFree nat (DayF day)
        = Day.dap . Day.trans2 nat . Day.trans1 nat $ day

-- |
-- Algebras of the same type as @'Free'@ monad is the class of all monads.
type instance AlgebraType  Free m = Monad m
type instance AlgebraType0 Free f = Functor f
-- |
-- @'Free'@ monad is free in the class of monad over the class of functors.
instance FreeAlgebra1 Free where
    liftFree = Free.liftF
    foldNatFree nat ff = Free.foldFree nat ff

type instance AlgebraType  Church.F m = Monad m
type instance AlgebraType0 Church.F f = Functor f
instance FreeAlgebra1 Church.F where
    liftFree  = Church.liftF
    foldNatFree = Church.foldF


type instance AlgebraType  Alt m = Alternative m
type instance AlgebraType0 Alt f = Functor f
instance FreeAlgebra1 Alt where
    liftFree  = Alt.liftAlt
    foldNatFree = Alt.runAlt

-- |
-- Algebras of the same type as @'L.StateT'@ monad is the class of all state
-- monads.
type instance AlgebraType  (L.StateT s) m = ( MonadState s m )
type instance AlgebraType0 (L.StateT s) m = Monad m
-- |
-- Lazy @'L.StateT'@ monad transformer is a free algebra in the class of monads
-- which satisfy the @'MonadState'@ constraint.  Note that this instance
-- captures that @'L.StateT' s@ is a monad transformer:
--
-- @
--  'liftFree' = 'lift'
-- @
--
-- This is also true for all the other monad transformers.
instance FreeAlgebra1 (L.StateT s) where
    liftFree = lift
    foldNatFree nat ma = do
        (a, s) <- get >>= nat . L.runStateT ma
        put s
        return a

-- |
-- Algebras of the same type as @'S.StateT'@ monad is the class of all state
-- monads.
type instance AlgebraType  (S.StateT s) m = ( MonadState s m )
type instance AlgebraType0 (S.StateT s) m = Monad m
-- |
-- Strict @'S.StateT'@ monad transformer is also a free algebra, thus @'hoistFreeH'@
-- is an isomorphism between the strict and lazy versions.
instance FreeAlgebra1 (S.StateT s) where
    liftFree :: Monad m => m a -> S.StateT s m a
    liftFree = lift
    foldNatFree nat ma = do
        (a, s) <- get >>= nat . S.runStateT ma
        put s
        return a

-- |
-- Algebras of the same type as @'L.WriterT'@ monad is the class of all writer
-- monads.
type instance AlgebraType  (L.WriterT w) m = ( MonadWriter w m )
type instance AlgebraType0 (L.WriterT w) m = ( Monad m, Monoid w )
-- |
-- Lazy @'L.WriterT'@ is free for algebras of type @'MonadWriter'@.
instance FreeAlgebra1 (L.WriterT w) where
    liftFree = lift
    foldNatFree nat (L.WriterT m) = fst <$> nat m

-- |
-- Algebras of the same type as @'S.WriterT'@ monad is the class of all writer
-- monads.
type instance AlgebraType  (S.WriterT w) m = ( MonadWriter w m )
type instance AlgebraType0 (S.WriterT w) m = ( Monad m, Monoid w )
-- |
-- Strict @'S.WriterT'@ monad transformer is a free algebra among all
-- @'MonadWriter'@s.
instance FreeAlgebra1 (S.WriterT w) where
    liftFree = lift
    foldNatFree nat (S.WriterT m) = fst <$> nat m

-- |
-- Algebras of the same type as @'L.ReaderT'@ monad is the class of all reader
-- monads.
type instance AlgebraType  (ReaderT r) m = ( MonadReader r m )
type instance AlgebraType0 (ReaderT r) m = ( Monad m )
-- |
-- @'ReaderT'@ is a free monad in the class of all @'MonadReader'@ monads.
instance FreeAlgebra1 (ReaderT r) where
    liftFree = lift
    foldNatFree nat (ReaderT g) =
        ask >>= nat . g

-- |
-- Algebras of the same type as @'S.ReaderT'@ monad is the class of all reader
-- monads.
type instance AlgebraType  (ExceptT e) m = ( MonadError e m )
type instance AlgebraType0 (ExceptT e) m = ( Monad m )
-- |
-- @'ExceptT' e@ is a free algebra among all @'MonadError' e@ monads.
instance FreeAlgebra1 (ExceptT e) where
    liftFree = lift
    foldNatFree nat (ExceptT m) = do
        ea <- nat m
        case ea of
            Left e  -> throwError e
            Right a -> return a

type instance AlgebraType  (L.RWST r w s) m = MonadRWS r w s m
type instance AlgebraType0 (L.RWST r w s) m = ( Monad m, Monoid w )
instance FreeAlgebra1 (L.RWST r w s) where
    liftFree = lift
    foldNatFree nat (L.RWST fn) = do
        r <- ask
        s <- get
        (a, s', w) <- nat $ fn r s
        put s'
        tell w
        return a

type instance AlgebraType  (S.RWST r w s) m = MonadRWS r w s m
type instance AlgebraType0 (S.RWST r w s) m = ( Monad m, Monoid w )
instance FreeAlgebra1 (S.RWST r w s) where
    liftFree = lift
    foldNatFree nat (S.RWST fn) = do
        r <- ask
        s <- get
        (a, s', w) <- nat $ fn r s
        put s'
        tell w
        return a
-- |
-- Algebra type for @'ListT'@ monad transformer.
class Monad m => MonadList m where
    mempty1 :: m a
    mappend1 :: m a -> m a -> m a

mappend1_ :: MonadList m => a -> a -> m a
mappend1_ a b = return a `mappend1` return b

instance Monad m => MonadList (ListT m) where
    mempty1 = ListT (return [])
    mappend1 (ListT ma) (ListT mb) = ListT $ mappend <$> ma <*> mb

type instance AlgebraType  ListT m = ( MonadList m )
type instance AlgebraType0 ListT f = ( Monad f )
instance FreeAlgebra1 ListT where
    liftFree = lift
    foldNatFree nat (ListT mas) = do
        as <- nat mas
        empty <- mempty1
        a <- foldM (\x y -> x `mappend1_` y) empty as
        return a

-- $monadContT
--
-- @'ContT' r m@ is not functorial in @m@, so there is no chance it can admit
-- an instance of @'FreeAlgebra1'@

-- |
-- A higher version @'Data.Algebra.Pointed'@ class.
--
-- With @'QuantifiedConstraints'@ this class will be redundant.
class MonadMaybe m where
    point :: forall a. m a

type instance AlgebraType  MaybeT m = ( Monad m, MonadMaybe m )
type instance AlgebraType0 MaybeT m = ( Monad m )
instance FreeAlgebra1 MaybeT where
    liftFree = lift
    foldNatFree nat (MaybeT mma) =
        nat mma >>= \ma -> case ma of
            Nothing -> point
            Just a  -> return a
