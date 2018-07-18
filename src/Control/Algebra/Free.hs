{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Algebra.Free
    ( AlgebraType1
    , FreeAlgebra1 (..)
    , foldFree1
    , hoistFree1
    , hoistFreeH
    , fmapFree1
    , joinFree1
    , bindFree1
    , assocFree1
    , iterFree1
    , DayF (..)
    , dayToAp
    , apToDay
    , MonadList (..)
    ) where

import           Control.Applicative.Free (Ap)
import qualified Control.Applicative.Free as Ap
import           Control.Monad (foldM)
import           Control.Monad.Except (ExceptT (..), MonadError (..))
import           Control.Monad.Free (Free)
import qualified Control.Monad.Free as Free
import           Control.Monad.List (ListT (..))
import           Control.Monad.Reader (MonadReader (..), ReaderT (..))
import           Control.Monad.RWS.Class (MonadRWS)
import           Control.Monad.RWS.Lazy as L (RWST (..))
import           Control.Monad.RWS.Strict as S (RWST (..))
import           Control.Monad.State.Class (MonadState (..))
import qualified Control.Monad.State.Lazy as L (StateT (..))
import qualified Control.Monad.State.Strict as S (StateT (..))
import           Control.Monad.Trans (lift)
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
-- * @'Data.Functor.Coyoneda.lowerCoyoneda' :: 'Functor' f => 'Coyoneda' f a -> f a@
-- * @'Control.Applicative.Free.retractAp' :: 'Applicative' f => 'Ap' f a -> f a@
-- * @'Control.Monad.Free.foldFree' :: 'Monad' m => (forall x. f x -> m x) -> 'Free' f a -> m a@
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
-- * @'Control.Applicative.Free.hoistAp' :: (forall a. f a -> g a) -> 'Ap' f b -> 'Ap' g b @
-- * @'Control.Monad.Free.hoistFree' :: 'Functor' g => (forall a. f a -> g a) -> 'Free' f b -> 'Free' g b@
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
--
-- prop> hoistFreeH . hoistFreeH = hoistFreeH
--
-- and when @'FreeAlgebra1' m ~ 'FreeAlgebra1' n@:
--
-- prop> hoistFreeH . hoistFreeH = id
hoistFreeH :: forall m n f a .
           ( AlgebraType m (n f)
           , AlgebraType1 m f
           , AlgebraType1 n f
           , FreeAlgebra1 m
           , FreeAlgebra1 n
           )
        => m f a
        -> n f a
hoistFreeH = foldMapFree1 returnFree1 id

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
bindFree1 :: forall m f g a .
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

-- |
-- Specialization of @'foldMapFree1' \@_ \@'Identity'@, e.g.
--
-- * @\\_ -> 'runIdentity' . 'Data.Functor.Coyoneda.lowerCoyoneda'@
-- * @'Control.Applicative.Free.iterAp' :: 'Functor' g => (g a -> a) -> 'Ap' g a -> a@
-- * @'Control.Monad.Free.iter' :: 'Functor' f => (f a -> a) -> 'Free' f a -> a@
iterFree1 :: forall m f a .
             ( FreeAlgebra1 m 
             , AlgebraType1 m f
             , AlgebraType m Identity
             )
          => (forall x . f x -> x)
          -> m f a
          -> a
iterFree1 f = runIdentity . foldMapFree1 @_ @Identity (Identity . f) id

-- Instances

-- |
-- Algebras of the same type as @'Coyoneda'@ are all functors.
type instance AlgebraType  Coyoneda g = Functor g
type instance AlgebraType1 Coyoneda g = ()
instance FreeAlgebra1 Coyoneda where
    returnFree1 = liftCoyoneda
    foldMapFree1 nat f (Coyoneda ba fx) = fmap f ba <$> nat fx

-- |
-- Algebras of the same type as @'Ap'@ are the applicative functors.
type instance AlgebraType  Ap g = Applicative g
type instance AlgebraType1 Ap g = Functor g
-- |
-- @'Ap'@ is a free in the class of applicative functors, over any functor
-- (@'Ap' f@ is applicative whenever @f@ is a functor)
instance FreeAlgebra1 Ap where
    returnFree1 = Ap.liftAp

    foldMapFree1 _   f (Ap.Pure a) = pure $ f a
    foldMapFree1 nat f (Ap.Ap fx apxa)
        = fmap f $ foldMapFree1 nat id apxa <*> nat fx

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
type instance AlgebraType1 DayF g = Functor g
-- |
-- @'DayF'@, as @'Ap'@ is a free applicative functor, but over applicative functors
-- (@'DayF' f@ is applicative if @f@ is an applicative functor).
instance FreeAlgebra1 DayF where
    returnFree1 fa = DayF $ Day fa fa const
    foldMapFree1 nat f (DayF day)
        = fmap f $ Day.dap . Day.trans2 nat . Day.trans1 nat $ day

-- |
-- Algebras of the same type as @'Free'@ monad is the class of all monads.
type instance AlgebraType  Free m = Monad m
type instance AlgebraType1 Free f = Functor f
-- |
-- @'Free'@ monad is free in the class of monad over the class of functors.
instance FreeAlgebra1 Free where
    returnFree1 = Free.liftF
    foldMapFree1 nat f ff = f <$> Free.foldFree nat ff

-- |
-- Algebras of the same type as @'L.StateT'@ monad is the class of all state
-- monads.
type instance AlgebraType  (L.StateT s) m = ( MonadState s m )
type instance AlgebraType1 (L.StateT s) m = Monad m
-- |
-- Lazy @'L.StateT'@ monad transformer is a free algebra in the class of monads
-- which satisfy the @'MonadState'@ constraint.  Note that this instance
-- captures that @'L.StateT' s@ is a monad transformer:
--
-- @
--  'returnFree1' = 'lift'
-- @
--
-- This is also true for all the other monad transformers.
instance FreeAlgebra1 (L.StateT s) where
    returnFree1 = lift
    foldMapFree1 nat f ma = do
        (a, s) <- get >>= nat . L.runStateT ma
        put s
        return (f a)

-- |
-- Algebras of the same type as @'S.StateT'@ monad is the class of all state
-- monads.
type instance AlgebraType  (S.StateT s) m = ( MonadState s m )
type instance AlgebraType1 (S.StateT s) m = Monad m
-- |
-- Strict @'S.StateT'@ monad transformer is also a free algebra, thus @'hoistFreeH'@
-- is an isomorphism between the strict and lazy versions.
instance FreeAlgebra1 (S.StateT s) where
    returnFree1 :: Monad m => m a -> S.StateT s m a
    returnFree1 = lift
    foldMapFree1 nat f ma = do
        (a, s) <- get >>= nat . S.runStateT ma
        put s
        return (f a)

-- |
-- Algebras of the same type as @'L.WriterT'@ monad is the class of all writer
-- monads.
type instance AlgebraType  (L.WriterT w) m = ( MonadWriter w m )
type instance AlgebraType1 (L.WriterT w) m = ( Monad m, Monoid w )
-- |
-- Lazy @'L.WriterT'@ is free for algebras of type @'MonadWriter'@.
instance FreeAlgebra1 (L.WriterT w) where
    returnFree1 = lift
    foldMapFree1 nat f (L.WriterT m) = do
        (a, w) <- nat m
        writer (f a, w)

-- |
-- Algebras of the same type as @'S.WriterT'@ monad is the class of all writer
-- monads.
type instance AlgebraType  (S.WriterT w) m = ( MonadWriter w m )
type instance AlgebraType1 (S.WriterT w) m = ( Monad m, Monoid w )
-- |
-- Strict @'S.WriterT'@ monad transformer is a free algebra among all
-- @'MonadWriter'@s.
instance FreeAlgebra1 (S.WriterT w) where
    returnFree1 = lift
    foldMapFree1 nat f (S.WriterT m) = do
        (a, w) <- nat m
        writer (f a, w)

-- |
-- Algebras of the same type as @'L.ReaderT'@ monad is the class of all reader
-- monads.
type instance AlgebraType  (ReaderT r) m = ( MonadReader r m )
type instance AlgebraType1 (ReaderT r) m = ( Monad m )
-- |
-- @'ReaderT'@ is a free monad in the class of all @'MonadReader'@ monads.
instance FreeAlgebra1 (ReaderT r) where
    returnFree1 = lift
    foldMapFree1 nat f (ReaderT g) =
        ask >>= fmap f . nat . g

-- |
-- Algebras of the same type as @'S.ReaderT'@ monad is the class of all reader
-- monads.
type instance AlgebraType  (ExceptT e) m = ( MonadError e m )
type instance AlgebraType1 (ExceptT e) m = ( Monad m )
-- |
-- @'ExceptT' e@ is a free algebra among all @'MonadError' e@ monads.
instance FreeAlgebra1 (ExceptT e) where
    returnFree1 = lift
    foldMapFree1 nat f (ExceptT m) = do
        ea <- nat m
        case ea of
            Left e  -> throwError e
            Right a -> return $ f a

type instance AlgebraType  (L.RWST r w s) m = MonadRWS r w s m
type instance AlgebraType1 (L.RWST r w s) m = ( Monad m, Monoid w )
instance FreeAlgebra1 (L.RWST r w s) where
    returnFree1 = lift
    foldMapFree1 nat f (L.RWST fn) = do
        r <- ask
        s <- get
        (a, s', w) <- nat $ fn r s
        put s'
        tell w
        return $ f a

type instance AlgebraType  (S.RWST r w s) m = MonadRWS r w s m
type instance AlgebraType1 (S.RWST r w s) m = ( Monad m, Monoid w )
instance FreeAlgebra1 (S.RWST r w s) where
    returnFree1 = lift
    foldMapFree1 nat f (S.RWST fn) = do
        r <- ask
        s <- get
        (a, s', w) <- nat $ fn r s
        put s'
        tell w
        return $ f a
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
type instance AlgebraType1 ListT f = ( Monad f )
instance FreeAlgebra1 ListT where
    returnFree1 = lift
    foldMapFree1 nat f (ListT mas) = do
        as <- nat mas
        empty <- mempty1
        a <- foldM (\x y -> x `mappend1_` y) empty as
        return $ f a

-- $monadContT
--
-- @'ContT' r m@ is not functorial in @m@, so there is no chance it can admit
-- an instance of @'FreeAlgebra1'@.
