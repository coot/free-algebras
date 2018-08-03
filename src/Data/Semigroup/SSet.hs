{- |
    Actions of [semigroup](https://en.wikipedia.org/wiki/Semigroup_action) (SSet).
 -}
module Data.Semigroup.SSet
    ( SSet (..)
    , rep
    , fact
    ) where

import           Data.Semigroup (Endo (..), Sum (..))
import           Data.Functor.Const (Const (..))
import           Data.Functor.Identity (Identity (..))
import qualified Data.Functor.Product as Functor (Product)
import qualified Data.Functor.Sum as Functor (Sum)
import           Data.Group (Group (..))
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Natural (Natural)
import           Data.Ord (Down)
import           Data.Set (Set)
import qualified Data.Set as Set

-- |
-- A lawful instance should satisfy:
--
-- prop> g `act` h `act` a = g <> h `act` a
--
-- This is the same as to say that `act` is a semigroup homomorphism from @s@ to
-- the monoid of endomorphisms of @a@ (i.e. maps from @a@ to @a@).
--
-- Note that if @g@ is a @'Group'@ then @'MAct' g@ is simply a @GSet@, this
-- is because monoids and groups share the same morphisms (a monoid homomorphis
-- between groups necessarily preserves inverses).
class Semigroup s => SSet s a where
    act :: s -> a -> a

rep :: SSet s a => s -> Endo a
rep s = Endo (act s)

instance Semigroup s => SSet s s where
    act = (<>)

instance (SSet s a, SSet s b) => SSet s (a, b) where
    act s (a, b) = (act s a, act s b)

instance (SSet s a, SSet s b, SSet s c) => SSet s (a, b, c) where
    act s (a, b, c) = (act s a, act s b, act s c)

instance (SSet s a, SSet s b, SSet s c, SSet s d) => SSet s (a, b, c, d) where
    act s (a, b, c, d) = (act s a, act s b, act s c, act s d)

instance (SSet s a, SSet s b, SSet s c, SSet s d, SSet s e) => SSet s (a, b, c, d, e) where
    act s (a, b, c, d, e) = (act s a, act s b, act s c, act s d, act s e)

instance (SSet s a, SSet s b, SSet s c, SSet s d, SSet s e, SSet s f) => SSet s (a, b, c, d, e, f) where
    act s (a, b, c, d, e, f) = (act s a, act s b, act s c, act s d, act s e, act s f)

instance (SSet s a, SSet s b, SSet s c, SSet s d, SSet s e, SSet s f, SSet s h) => SSet s (a, b, c, d, e, f, h) where
    act s (a, b, c, d, e, f, h) = (act s a, act s b, act s c, act s d, act s e, act s f, act s h)

instance (SSet s a, SSet s b, SSet s c, SSet s d, SSet s e, SSet s f, SSet s h, SSet s i) => SSet s (a, b, c, d, e, f, h, i) where
    act s (a, b, c, d, e, f, h, i) = (act s a, act s b, act s c, act s d, act s e, act s f, act s h, act s i)

instance SSet s a => SSet s [a] where
    act s = map (act s)

instance SSet s a => SSet s (NonEmpty a) where
    act s as = NE.map (act s) as

instance (SSet s a, Ord a) => SSet s (Set a) where
    act s as = Set.map (act s) as

-- |
-- Any @'SSet'@ wrapped in a functor is a valid @'SSet'@.
fact :: (Functor f, SSet s a) => s -> f a -> f a
fact s = fmap (act s)

instance SSet s a => SSet s (Identity a) where
    act = fact

instance SSet s a => SSet (Identity s) a where
    act (Identity f) a = f `act` a

instance SSet s a => SSet s (Maybe a) where
    act = fact

instance SSet s b => SSet s (Either a b) where
    act = fact

instance SSet s a => SSet s (Down a) where
    act = fact 
instance SSet s a => SSet s (IO a) where
    act = fact

instance SSet s b => SSet s (a -> b) where
    act = fact

instance SSet (Endo a) a where
    act (Endo f) a = f a

instance Monoid s => SSet (Sum Natural) s where
    act (Sum 0) _ = mempty
    act (Sum n) s = s <> act (Sum (n - 1)) s

instance Group g => SSet (Sum Integer) g where
    act (Sum n) g | n < 0      = invert g <> act (Sum (n + 1)) g
                  | n > 0      = g <> act (Sum (n - 1)) g
                  | otherwise  = mempty

instance SSet s a => SSet s (Const a b) where
    act s (Const a) = Const $ s `act` a

instance (Functor f, Functor h, SSet s a) => SSet s (Functor.Product f h a) where
    act = fact

instance (Functor f, Functor h, SSet s a) => SSet s (Functor.Sum f h a) where
    act = fact
