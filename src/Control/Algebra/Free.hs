{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE UndecidableInstances       #-}

-- 'ListT' transformer is depreciated
{-# OPTIONS_GHC -Wno-deprecations       #-}

module Control.Algebra.Free
    ( -- Higher free algebra class
      FreeAlgebra1 (..)
      -- ** Type level witnesses
    , Proof (..)
      -- ** Higher algebra type \/ constraints
    , AlgebraType0
    , AlgebraType
      -- * Combinators
    , wrapFree
    , foldFree1
    , unFoldNatFree
    , hoistFree1
    , hoistFreeH
    , joinFree1
    , bindFree1
    , assocFree1
    , iterFree1
    , cataFree1
      -- * Day convolution
    , DayF (..)
    , dayToAp
    , apToDay

      -- * Free construction in continuation passing style
    , Free1 (..)
      -- * Various classes (higher algebra types)
    , MonadList (..)
    , MonadMaybe (..)

    ) where

import           Control.Applicative ( Alternative (..)
                                     , liftA2
                                     )
import           Control.Applicative.Free (Ap)
import qualified Control.Applicative.Free as Ap
import qualified Control.Applicative.Free.Fast as Fast
import qualified Control.Applicative.Free.Final as Final
import           Control.Alternative.Free (Alt (..))
import qualified Control.Alternative.Free as Alt
import           Control.Monad ( MonadPlus (..), foldM, join)
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
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Monad.Writer.Class (MonadWriter (..))
import qualified Control.Monad.Writer.Lazy as L (WriterT (..))
import qualified Control.Monad.Writer.Strict as S (WriterT (..))
import           Control.Monad.Zip (MonadZip (..))
import           Data.Kind (Constraint, Type)
import           Data.Fix (Fix, cataM)
import           Data.Functor.Coyoneda (Coyoneda (..), liftCoyoneda)
import           Data.Functor.Day (Day (..))
import qualified Data.Functor.Day as Day
import           Data.Functor.Identity (Identity (..))

import           Data.Algebra.Free (AlgebraType, AlgebraType0, Proof (..))

-- | Higher kinded version of @'FreeAlgebra'@.  Instances includes free functors,
-- free applicative functors, free monads, state monads etc.
--
-- A lawful instance should guarantee that @'foldNatFree'@ is an isomorphism
-- with inverses @'unFoldNatFree'@.
--
-- This guaranties that @m@ is a left adjoint functor from the category of
-- types of kind @Type -> Type@ which satisfy @'AlgebraType0' m@ constraint, to the
-- category of types of kind @Type -> Type@ which satisfy the @'AlgebraType' m@
-- constraint.  This functor is left adjoin to the forgetful functor (which is
-- well defined if the laws on @'AlgebraType0'@ family are satisfied.  This in
-- turn guarantees that @m@ composed with this forgetful functor is a monad.
-- In result we get monadic operations:
-- 
--   * @return = 'liftFree'@
--   * @(>>=)  = 'bindFree1'@
--   * @join   = 'joinFree1'@
--
-- For @m@ such that @'AlgebraType0'@ subsumes @'Monad'@ this class implies:
--
-- * @MFunctor@ via @hoist = hoistFree1@
-- * @MMonad@ via @embed = flip bindFree1@
-- * @MonadTrans@ via @lift = liftFree@
--
class FreeAlgebra1 (m :: (k -> Type) -> k -> Type) where

    {-# MINIMAL liftFree, foldNatFree #-}

    -- | Natural transformation that embeds generators into @m@.
    liftFree :: AlgebraType0 m f => f a -> m f a

    -- | The freeness property.
    --
    -- prop> foldNatFree nat (liftFree m) = nat m
    -- prop> foldNatFree nat . foldNatFree nat' = foldNatFree (foldNatFree nat . nat')
    --
    foldNatFree
        :: forall d f a .
           ( AlgebraType  m d
           , AlgebraType0 m f
           )
        => (forall x. f x -> d x)
        -- ^ a natural transformation which embeds generators of @m@ into @d@
        -> (m f a -> d a)
        -- ^ a morphism from @m f@ to @d@

    -- | A proof that @'AlgebraType' m (m f)@ holds for all @AlgebraType0 f => f@.
    -- Together with @'hoistFree1'@ this proves that @FreeAlgebra m => m@ is
    -- a functor from the full subcategory of types of kind @Type -> Type@
    -- which satisfy @'AlgebraType0' m f@ to ones that satisfy @'AlgebraType'
    -- m f@.
    --
    codom1  :: forall f. AlgebraType0 m f => Proof (AlgebraType m (m f)) (m f)

    default codom1 :: forall a. AlgebraType m (m a)
                   => Proof (AlgebraType m (m a)) (m a)
    codom1 = Proof

    -- | A proof that the forgetful functor from the full subcategory of types of
    -- kind @Type -> Type@ satisfying @'AlgebraType' m f@ constraint to types
    -- satisfying @'AlgebraType0' m f@ is well defined.
    --
    forget1 :: forall f. AlgebraType  m f => Proof (AlgebraType0 m f) (m f)

    default forget1 :: forall a. AlgebraType0 m a
                    => Proof (AlgebraType0 m a) (m a)
    forget1 = Proof

-- | Anything that carries @'FreeAlgebra1'@ constraint is also an instance of
-- @'Control.Monad.Free.Class.MonadFree'@, but not vice versa. You can use
-- @'wrap'@ to define a @'Control.Monad.Free.Class.MonadFree'@ instance.
-- @'ContT'@ is an example of a monad which does have an  @'FreeAlgebra1'@
-- instance, but has an @'MonadFree'@ instance.
--
-- The @'Monad'@ constrain will be satisfied for many monads through the
-- @'AlgebraType m'@ constraint.
--
wrapFree
    :: forall (m :: (Type -> Type) -> Type -> Type)
              (f :: Type -> Type) 
              a .
       ( FreeAlgebra1 m
       , AlgebraType0 m f
       , Monad (m f)
       )
    => f (m f a)
    -> m f a
wrapFree = join . liftFree
{-# INLINABLE wrapFree #-}

-- | @'FreeAlgebra1' m@ implies that @m f@ is a foldable.
--
-- @
--  'foldFree1' . 'liftFree' == 'id' :: f a -> f a
-- @
--
-- @foldFree1@ is the
-- [unit](https://ncatlab.org/nlab/show/unit+of+an+adjunction) of the
-- adjunction imposed by @FreeAlgebra1@ constraint.
--
-- It can be specialized to:
--
-- * @'Data.Functor.Coyoneda.lowerCoyoneda' :: 'Functor' f => 'Coyoneda' f a -> f a@
-- * @'Control.Applicative.Free.retractAp' :: 'Applicative' f => 'Ap' f a -> f a@
-- * @'Control.Monad.Free.retract' :: 'Monad' f => 'Free' f a -> f a@
--
foldFree1 :: forall m f a .
             ( FreeAlgebra1 m
             , AlgebraType  m f
             )
          => m f a
          -> f a
foldFree1 = case forget1 :: Proof (AlgebraType0 m f) (m f) of
    Proof -> foldNatFree id
{-# INLINABLE foldFree1 #-}

-- | @'unFoldNatFree'@ is an inverse of @'foldNatFree'@
--
-- It is uniquely determined by its universal property (by Yoneda lemma):
--
-- prop> unFoldNatFree id = ruturnFree1
--
-- Note that @'unFoldNatFree' id@ is the
-- [unit](https://ncatlab.org/nlab/show/unit+of+an+adjunction) of the
-- adjunction imposed by the @'FreeAlgebra1'@ constraint.
--
unFoldNatFree
    :: ( FreeAlgebra1 m
       , AlgebraType0 m f
       )
    => (forall x . m f x -> d x)
    -> f a -> d a
unFoldNatFree nat = nat . liftFree

-- | This is a functor instance for @m@ when considered as an endofuctor of some
-- subcategory of @Type -> Type@ (e.g. endofunctors of /Hask/) and it satisfies
-- the functor laws:
--
-- prop> hoistFree1 id = id
-- prop> hoistFree1 f . hoistFree1 g = hoistFree1 (f . g)
--
-- It can be specialized to:
--
-- * @'Control.Applicative.Free.hoistAp' :: (forall a. f a -> g a) -> 'Ap' f b -> 'Ap' g b @
-- * @'Control.Monad.Free.hoistFree' :: 'Functor' g => (forall a. f a -> g a) -> 'Free' f b -> 'Free' g b@
-- * @Control.Monad.Morph.hoist@ for @'FreeAlgebra1' m => m@ such that
--   @'AlgebraType0' m@ subsumes @Monad m@, e.g.
--   @'Control.Monad.State.Lazy.StateT'@, @'Control.Monad.Writer.Lazy.WriterT'@
--   or @'Control.Monad.Reader.ReaderT'@.
--
hoistFree1 :: forall m f g a .
              ( FreeAlgebra1 m
              , AlgebraType0 m g
              , AlgebraType0 m f
              )
           => (forall x. f x -> g x) -- ^ a natural transformation @f ~> g@
           -> m f a
           -> m g a
hoistFree1 nat = case codom1 :: Proof (AlgebraType m (m g)) (m g) of
    Proof -> foldNatFree (liftFree . nat)
{-# INLINABLE [1] hoistFree1 #-}

{-# RULES

"hositFree1/foldNatFree"
    forall (nat  :: forall (x :: k).  g x -> c x)
           (nat0 :: forall (x :: k). f x -> g x)
           (f :: m f a).
    foldNatFree nat (hoistFree1 nat0 f) = foldNatFree (nat . nat0) f

#-}

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
           ( FreeAlgebra1 m
           , FreeAlgebra1 n
           , AlgebraType0 m f
           , AlgebraType0 n f
           , AlgebraType  m (n f)
           )
        => m f a
        -> n f a
hoistFreeH = foldNatFree liftFree
{-# INLINABLE [1] hoistFreeH #-}

{-# RULES

"hoistFreeH/foldNatFree" forall (nat :: forall (x :: k). f x -> c x)
                                (f :: AlgebraType m c => m f a).
                         foldNatFree nat (hoistFreeH f) = foldNatFree nat f
#-}

-- | @'joinFree1'@ makes @m@ a monad in some subcatgory of types of kind @Type -> Type@
-- (usually the endo-functor category of @Hask@).  It is just a specialization
-- of @'foldFree1'@.
--
joinFree1 :: forall m f a .
             ( FreeAlgebra1 m
             , AlgebraType0 m f
             )
          => m (m f) a
          -> m f a
joinFree1 = case codom1 :: Proof (AlgebraType m (m f)) (m f) of
    Proof -> case forget1 :: Proof (AlgebraType0 m (m f)) (m (m f)) of
        Proof -> foldFree1
{-# INLINABLE joinFree1 #-}

-- | Bind operator for the @'joinFree1'@ monad, this is just @'foldNatFree'@ in
-- disguise.
--
-- For @'Control.Monad.State.Lazy.StateT'@,
-- @'Control.Monad.Writer.Lazy.WriterT'@ or
-- @'Control.Monad.Reader.Lazy.ReaderT'@ (or any @'FreeAlgebra1' m => m@ such
-- that @'AlgebraType0' m@ subsumes @'Monad' m@), this is the @>>=@ version of
-- @Control.Monad.Morph.embed@.
--
bindFree1 :: forall m f g a .
             ( FreeAlgebra1 m
             , AlgebraType0 m g
             , AlgebraType0 m f
             )
          => m f a
          -> (forall x . f x -> m g x) -- ^ natural transformation @f ~> m g@
          -> m g a
bindFree1 mfa nat = case codom1 :: Proof (AlgebraType m (m g)) (m g) of
    Proof -> foldNatFree nat mfa
{-# INLINABLE bindFree1 #-}

assocFree1 :: forall m f a .
              ( FreeAlgebra1 m
              , AlgebraType  m f
              , Functor (m (m f))
              )
           => m f (m f a)
           -> m (m f) (f a)
assocFree1 = case forget1 :: Proof (AlgebraType0 m f) (m f) of
    Proof -> case codom1 :: Proof (AlgebraType m (m f)) (m f) of
        Proof -> case forget1 :: Proof (AlgebraType0 m (m f)) (m (m f)) of
            Proof -> case codom1 :: Proof (AlgebraType m (m (m f))) (m (m f)) of
                Proof -> fmap foldFree1 . foldNatFree (hoistFree1 liftFree . liftFree)
{-# INLINABLE assocFree1 #-}

-- | @'Fix' (m f)@ is the initial /algebra/ of type @'AlgebraType' m@ and
-- @'AlgebraType0' f@.
--
cataFree1 :: forall m f a .
             ( FreeAlgebra1 m
             , AlgebraType  m f
             , Monad f
             , Traversable (m f)
             )
          => Fix (m f)
          -> f a
cataFree1 = cataM foldFree1

-- | Specialization of @'foldNatFree' \@_ \@'Identity'@; it will further specialize to:
--
-- * @\\_ -> 'runIdentity' . 'Data.Functor.Coyoneda.lowerCoyoneda'@
-- * @'Control.Applicative.Free.iterAp' :: 'Functor' g => (g a -> a) -> 'Ap' g a -> a@
-- * @'Control.Monad.Free.iter' :: 'Functor' f => (f a -> a) -> 'Free' f a -> a@
--
iterFree1 :: forall m f a .
             ( FreeAlgebra1 m
             , AlgebraType0 m f
             , AlgebraType m Identity
             )
          => (forall x . f x -> x)
          -> m f a
          -> a
iterFree1 f = runIdentity . foldNatFree (Identity . f)
{-# INLINABLE iterFree1 #-}

-- Instances

-- | Algebras of the same type as @'Coyoneda'@ are all functors.
--
type instance AlgebraType0 Coyoneda g = ()
type instance AlgebraType  Coyoneda g = Functor g
instance FreeAlgebra1 Coyoneda where
    liftFree = liftCoyoneda
    foldNatFree nat (Coyoneda ba fx) = ba <$> nat fx

-- | Algebras of the same type as @'Ap'@ are the applicative functors.
--
type instance AlgebraType0 Ap g = Functor g
type instance AlgebraType  Ap g = Applicative g
-- | @'Ap'@ is a free in the class of applicative functors, over any functor
-- (@'Ap' f@ is applicative whenever @f@ is a functor)
--
instance FreeAlgebra1 Ap where
    liftFree  = Ap.liftAp
    foldNatFree = Ap.runAp

type instance AlgebraType0 Fast.Ap g = Functor g
type instance AlgebraType  Fast.Ap g = Applicative g
instance FreeAlgebra1 Fast.Ap where
    liftFree  = Fast.liftAp
    foldNatFree = Fast.runAp

type instance AlgebraType0 Final.Ap g = Functor g
type instance AlgebraType  Final.Ap g = Applicative g
instance FreeAlgebra1 Final.Ap where
    liftFree  = Final.liftAp
    foldNatFree = Final.runAp

-- | @'Day' f f@ newtype wrapper.  It is isomorphic with @'Ap' f@ for
-- applicative functors @f@ via @'dayToAp'@ (and @'apToDay'@).
--
newtype DayF f a = DayF { runDayF :: Day f f a}
    deriving (Functor, Applicative)

dayToAp :: Applicative f => Day f f a -> Ap f a
dayToAp =  hoistFreeH . DayF

apToDay :: Applicative f => Ap f a -> Day f f a
apToDay = runDayF . hoistFreeH

-- | Algebras of the same type as @'DayF'@ are all the applicative functors.
--
type instance AlgebraType0 DayF g = Applicative g
type instance AlgebraType  DayF g = Applicative g
-- | @'DayF'@, as @'Ap'@ is a free applicative functor, but over applicative functors
-- (@'DayF' f@ is applicative if @f@ is an applicative functor).
--
instance FreeAlgebra1 DayF where
    liftFree fa = DayF $ Day fa fa const
    foldNatFree nat (DayF day)
        = Day.dap . Day.trans2 nat . Day.trans1 nat $ day

-- | Algebras of the same type as @'Free'@ monad is the class of all monads.
--
type instance AlgebraType0 Free f = Functor f
type instance AlgebraType  Free m = Monad m
-- | @'Free'@ monad is free in the class of monad over the class of functors.
--
instance FreeAlgebra1 Free where
    liftFree    = Free.liftF
    foldNatFree = Free.foldFree

type instance AlgebraType0 Church.F f = Functor f
type instance AlgebraType  Church.F m = Monad m
instance FreeAlgebra1 Church.F where
    liftFree    = Church.liftF
    foldNatFree = Church.foldF

type instance AlgebraType0 Alt f = Functor f
type instance AlgebraType  Alt m = Alternative m
instance FreeAlgebra1 Alt where
    liftFree    = Alt.liftAlt
    foldNatFree = Alt.runAlt

-- | Algebras of the same type as @'L.StateT'@ monad is the class of all state
-- monads.
--
type instance AlgebraType0 (L.StateT s) m = Monad m
type instance AlgebraType  (L.StateT s) m = ( MonadState s m )
-- | Lazy @'L.StateT'@ monad transformer is a free algebra in the class of monads
-- which satisfy the @'MonadState'@ constraint.  Note that this instance
-- captures that @'L.StateT' s@ is a monad transformer:
--
-- @
--  'liftFree' = 'lift'
-- @
--
-- This is also true for all the other monad transformers.
--
instance FreeAlgebra1 (L.StateT s) where
    liftFree = lift
    foldNatFree nat ma = do
        (a, s) <- get >>= nat . L.runStateT ma
        put s
        return a

-- | Algebras of the same type as @'S.StateT'@ monad is the class of all state
-- monads.
--
type instance AlgebraType0 (S.StateT s) m = Monad m
type instance AlgebraType  (S.StateT s) m = ( MonadState s m )
-- | Strict @'S.StateT'@ monad transformer is also a free algebra, thus
-- @'hoistFreeH'@ is an isomorphism between the strict and lazy versions.
--
instance FreeAlgebra1 (S.StateT s) where
    liftFree :: Monad m => m a -> S.StateT s m a
    liftFree = lift
    foldNatFree nat ma = do
        (a, s) <- get >>= nat . S.runStateT ma
        put s
        return a

-- | Algebras of the same type as @'L.WriterT'@ monad is the class of all
-- writer monads.
--
type instance AlgebraType0 (L.WriterT w) m = ( Monad m, Monoid w )
type instance AlgebraType  (L.WriterT w) m = ( MonadWriter w m )
-- | Lazy @'L.WriterT'@ is free for algebras of type @'MonadWriter'@.
--
instance FreeAlgebra1 (L.WriterT w) where
    liftFree = lift
    foldNatFree nat (L.WriterT m) = fst <$> nat m

-- | Algebras of the same type as @'S.WriterT'@ monad is the class of all
-- writer monads.
--
type instance AlgebraType0 (S.WriterT w) m = ( Monad m, Monoid w )
type instance AlgebraType  (S.WriterT w) m = ( MonadWriter w m )
-- | Strict @'S.WriterT'@ monad transformer is a free algebra among all
-- @'MonadWriter'@s.
--
instance FreeAlgebra1 (S.WriterT w) where
    liftFree = lift
    foldNatFree nat (S.WriterT m) = fst <$> nat m

-- | Algebras of the same type as @'L.ReaderT'@ monad is the class of all
-- reader monads.
--
-- TODO: take advantage of poly-kinded `ReaderT`
--
type instance AlgebraType0 (ReaderT r) m = ( Monad m )
type instance AlgebraType  (ReaderT r) m = ( MonadReader r m )
-- | @'ReaderT'@ is a free monad in the class of all @'MonadReader'@ monads.
--
instance FreeAlgebra1 (ReaderT r :: (Type -> Type) -> Type -> Type) where
    liftFree = lift
    foldNatFree nat (ReaderT g) =
        ask >>= nat . g

-- | Algebras of the same type as @'S.ReaderT'@ monad is the class of all
-- reader monads.
--
type instance AlgebraType0 (ExceptT e) m = ( Monad m )
type instance AlgebraType  (ExceptT e) m = ( MonadError e m )
-- | @'ExceptT' e@ is a free algebra among all @'MonadError' e@ monads.
--
instance FreeAlgebra1 (ExceptT e) where
    liftFree = lift
    foldNatFree nat (ExceptT m) = do
        ea <- nat m
        case ea of
            Left e  -> throwError e
            Right a -> return a

type instance AlgebraType0 (L.RWST r w s) m = ( Monad m, Monoid w )
type instance AlgebraType  (L.RWST r w s) m = MonadRWS r w s m
instance FreeAlgebra1 (L.RWST r w s) where
    liftFree = lift
    foldNatFree nat (L.RWST fn) = do
        r <- ask
        s <- get
        (a, s', w) <- nat $ fn r s
        put s'
        tell w
        return a

type instance AlgebraType0 (S.RWST r w s) m = ( Monad m, Monoid w )
type instance AlgebraType  (S.RWST r w s) m = MonadRWS r w s m
instance FreeAlgebra1 (S.RWST r w s) where
    liftFree = lift
    foldNatFree nat (S.RWST fn) = do
        r <- ask
        s <- get
        (a, s', w) <- nat $ fn r s
        put s'
        tell w
        return a

-- | Algebra type for @'ListT'@ monad transformer.
--
class Monad m => MonadList m where
    mempty1 :: m a
    mappend1 :: m a -> m a -> m a

mappend1_ :: MonadList m => a -> a -> m a
mappend1_ a b = return a `mappend1` return b
{-# INLINABLE mappend1_ #-}

instance Monad m => MonadList (ListT m) where
    mempty1 = ListT (return [])
    mappend1 (ListT ma) (ListT mb) = ListT $ mappend <$> ma <*> mb

type instance AlgebraType0 ListT f = ( Monad f )
type instance AlgebraType  ListT m = ( MonadList m )
instance FreeAlgebra1 ListT where
    liftFree = lift
    foldNatFree nat (ListT mas) = do
        as <- nat mas
        empty1 <- mempty1
        foldM (\x y -> x `mappend1_` y) empty1 as

-- | Free construction for kinds @'Type' -> 'Type'@.  @'Free1' 'Functor'@ is
-- isomorphic to @'Coyoneda'@ via @'hoistFreeH'@, and @'Free1' 'Applicative'@
-- is isomorphic to @'Ap'@ (also via @'hoistFreeH'@).
--
-- Note: useful instance are only provided for ghc-8.6 using quantified
-- constraints.
--
newtype Free1 (c :: (Type -> Type) -> Constraint)
              (f ::  Type -> Type)
              a
      = Free1 {
          runFree1 :: forall g. c g => (forall x. f x -> g x) -> g a
        }

--
-- instances for @'Free1'@ using quantified constraints
--

-- | @'Free1'@ is a functor whenever @c f@ implies @'Functor' f@ .
--
instance (forall h. c h => Functor h)
         => Functor (Free1 c f) where

    fmap :: forall a b. (a -> b) -> Free1 c f a -> Free1 c f b
    fmap f (Free1 g) = Free1 $ \h -> fmap f (g h)

    a <$ Free1 g = Free1 $ \h -> a <$ g h

-- | @'Free1'@ is an applicative functor whenever @c f@ implies @'Applicative'
-- f@.
--
instance (forall h. c h => Applicative h, c (Free1 c f))
         => Applicative (Free1 c f) where

    pure a = Free1 $ \_ -> pure a

    Free1 f <*> Free1 g = Free1 $ \h -> f h <*> g h

    liftA2 f (Free1 x) (Free1 y) = Free1 $ \h -> liftA2 f (x h) (y h)

    Free1 f *> Free1 g = Free1 $ \h -> f h *> g h

    Free1 f <* Free1 g = Free1 $ \h -> f h <* g h


-- | @'Free1'@ is a monad whenever @c f@ implies @'Monad' f@.
--
instance (forall h. c h => Monad h, c (Free1 c f))
         => Monad (Free1 c f) where

    return = pure

    Free1 f >>= k = Free1 $ \h ->
        f h >>= (\a -> case k a of Free1 l -> l h)


instance (forall h. c h => Alternative h, c (Free1 c f))
         => Alternative (Free1 c f) where
    empty = Free1 $ \_ -> empty

    Free1 f <|> Free1 g = Free1 $ \h -> f h <|> g h

    some (Free1 f) = Free1 $ \h -> some (f h)

    many (Free1 f) = Free1 $ \h -> many (f h)


instance (forall h. c h => MonadPlus h, c (Free1 c f))
         => MonadPlus (Free1 c f) where

    mzero = Free1 $ \_ -> mzero

    Free1 f `mplus` Free1 g = Free1 $ \h -> f h `mplus` g h 


instance (forall h. c h => MonadZip h, c (Free1 c f))
         => MonadZip (Free1 c f) where

    Free1 f `mzip` Free1 g = Free1 $ \h -> f h `mzip` g h

    mzipWith k (Free1 f) (Free1 g) = Free1 $ \h -> mzipWith k (f h) (g h)

    munzip (Free1 f) = (Free1 $ \h -> fst (munzip (f h)), Free1 $ \h -> snd (munzip (f h)))


type instance AlgebraType0 (Free1 c) f = ()
type instance AlgebraType  (Free1 c) f = (c f)
instance (forall f. c (Free1 c f)) => FreeAlgebra1 (Free1 c) where

    liftFree = \fa -> Free1 $ \g -> g fa

    foldNatFree nat (Free1 f) = f nat

-- $monadContT
--
-- @'ContT' r m@ is not functorial in @m@, so there is no chance it can admit
-- an instance of @'FreeAlgebra1'@

-- | A higher version @'Data.Algebra.Pointed'@ class.
--
-- With @'QuantifiedConstraints'@ this class will be redundant.
class MonadMaybe m where
    point :: forall a. m a

instance Monad m => MonadMaybe (MaybeT m) where
    point = MaybeT (return Nothing)

type instance AlgebraType0 MaybeT m = ( Monad m )
type instance AlgebraType  MaybeT m = ( Monad m, MonadMaybe m )
instance FreeAlgebra1 MaybeT where
    liftFree = lift
    foldNatFree nat (MaybeT mma) =
        nat mma >>= \ma -> case ma of
            Nothing -> point
            Just a  -> return a
