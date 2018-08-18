{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Control.Category.Free where

import           Prelude hiding (id, (.))

import           Control.Arrow (Kleisli (..))
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.List.NonEmpty as NE

import           Control.Category (Category (..))

-- |
-- Free category
data Cat :: (* -> * -> *) -> * -> * -> * where
    Id :: Cat f a a
    (:.:) :: f b c -> Cat f a b -> Cat f a c

instance Category (Cat f) where
    id = Id
    Id . ys = ys
    (x :.: xs) . ys = x :.: (xs . ys)

liftCat :: f a b -> Cat f a b
liftCat fab = fab :.: Id

foldFunCat
    :: forall f g a b .  Category g
    => (forall x y. f x y -> g x y)
    -> Cat f a b
    -> g a b
foldFunCat _ Id = id
foldFunCat fun (fbc :.: cab) = fun fbc . foldFunCat fun cab

foldFunKleisli
    :: forall f m a b . Monad m
    => (forall x y. f x y -> Kleisli m x y)
    -> Cat f a b
    -> Kleisli m a b
foldFunKleisli = foldFunCat

unFoldFunCat
    :: (Cat f a b -> c)
    -> f a b
    -> c
unFoldFunCat f = f . liftCat

foldCat
    :: forall c x y .
       Category c
    => Cat c x y
    -> c x y
foldCat cc = foldFunCat id cc

fmapCat :: forall c d x y .
            (forall u v. c u v -> d u v)
         -> Cat c x y
         -> Cat d x y
fmapCat fun cxy = foldFunCat (liftCat . fun) cxy

joinCat :: forall c x y .
            Cat (Cat c) x y
         -> Cat c x y
joinCat = foldCat

bindCat :: forall c d x y .
            Cat c x y
   -> (forall u v. c u v -> Cat d u v)
   -> Cat d x y
bindCat cc fun = foldFunCat fun cc


-- Send & Recv using free category

data Chan snd rcv = Chan (MVar (NonEmpty snd)) (MVar (NonEmpty rcv))

-- |
-- SendRecv graph which we use to generate a category.
data SendRecv x y where
    Send :: Chan snd rcv -> SendRecv snd ()
    Recv :: SendRecv (Chan snd rcv) rcv

class Category c => SendRecvCat c where
    send :: Chan snd rcv -> c snd ()
    recv :: c (Chan snd rcv) rcv

-- |
-- An abstract instance
instance SendRecvCat (Cat SendRecv) where
    send snd_ = Send snd_ :.: Id
    recv = Recv :.: Id

-- |
-- A concrete implementatino in @IO@
instance SendRecvCat (Kleisli IO) where
    send (Chan snd_ _) = Kleisli $ \a -> MVar.modifyMVar_ snd_ (return . NE.cons a)
    recv = Kleisli fn
        where
        fn :: Chan snd rcv -> IO rcv
        fn (Chan _ rcv) = do
            as <- MVar.takeMVar rcv
            case as of
                a :| (a' : as') -> do
                    MVar.putMVar rcv (a' :| as')
                    return a
                a :| [] -> return a
