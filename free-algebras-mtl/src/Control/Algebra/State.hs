module Control.Algebra.State
    ( StateAlgebra (..)
    ) where

import           Control.Monad.Trans (lift)
import           Control.Monad.State.Strict (StateT (..))

import           Data.Algebra.Free (AlgebraType)
import           Control.Algebra.Free (FreeAlgebra1 (..), AlgebraType1)


class Monad m => StateAlgebra (m :: * -> *) s | m -> s where
    get :: m s
    put :: s -> m ()

type instance AlgebraType  (StateT s) m = ( Monad m, StateAlgebra m s )
type instance AlgebraType1 (StateT s) m = Monad m

-- |
-- `StateT` transformer is a free algebra in the class of monads which satisfy
-- the  `StateAlgebra` constraint.  Note that this instance captures that
-- `StateT s` is a monad transformer, since
--
-- prop> returnFree1 = lift
--  
instance FreeAlgebra1 (StateT s) where
    returnFree1 :: Monad m => m a -> StateT s m a
    returnFree1 = lift

    foldMapFree1 :: forall m s d a b .
                    ( Monad m
                    , StateAlgebra d s
                    )
                 => (forall x . m x -> d x)
                 -> (a -> b)
                 -> StateT s m a
                 -> d b
    foldMapFree1 nat f ma = do
        (a, s) <- get >>= runStateT (natS ma)
        put s
        return (f a)
        where
            natS :: StateT s m a -> StateT s d a
            natS (StateT g) = StateT $ \s -> nat (g s)
