module Data.GSet where

import           Control.Monad (ap)
import           Data.Group (Group)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Set (Set)
import qualified Data.Set as Set

import           Data.Algebra.Free (AlgebraType, FreeAlgebra (..), bindFree)

-- |
-- Lawful instance should satisfy:
-- @
--    act 'mempty' = 'id'
--    g `act` h `act` a = g <> h `act` a
-- @
-- This is the same as to say that `act` is a group homomorphism from @G@ to
-- the group of automorphisms of @a@ (i.e. invertible maps from @a@ to @a@).
class Group g => GSet g a where
    act :: g -> a -> a

instance Group g => GSet g g where
    act = (<>)

instance (GSet g a, GSet g b) => GSet g (a, b) where
    act g (a, b) = (act g a, act g b)

instance (GSet g a, GSet g b, GSet g c) => GSet g (a, b, c) where
    act g (a, b, c) = (act g a, act g b, act g c)

instance (GSet g a, GSet g b, GSet g c, GSet g d) => GSet g (a, b, c, d) where
    act g (a, b, c, d) = (act g a, act g b, act g c, act g d)

instance (GSet g a, GSet g b, GSet g c, GSet g d, GSet g e) => GSet g (a, b, c, d, e) where
    act g (a, b, c, d, e) = (act g a, act g b, act g c, act g d, act g e)

instance (GSet g a, GSet g b, GSet g c, GSet g d, GSet g e, GSet g f) => GSet g (a, b, c, d, e, f) where
    act g (a, b, c, d, e, f) = (act g a, act g b, act g c, act g d, act g e, act g f)

instance (GSet g a, GSet g b, GSet g c, GSet g d, GSet g e, GSet g f, GSet g h) => GSet g (a, b, c, d, e, f, h) where
    act g (a, b, c, d, e, f, h) = (act g a, act g b, act g c, act g d, act g e, act g f, act g h)

instance (GSet g a, GSet g b, GSet g c, GSet g d, GSet g e, GSet g f, GSet g h, GSet g i) => GSet g (a, b, c, d, e, f, h, i) where
    act g (a, b, c, d, e, f, h, i) = (act g a, act g b, act g c, act g d, act g e, act g f, act g h, act g i)

instance GSet g a => GSet g [a] where
    act g as = map (act g) as

instance GSet g a => GSet g (NonEmpty a) where
    act g as = NE.map (act g) as

instance (GSet g a, Ord a) => GSet g (Set a) where
    act g m = Set.map (act g) m

{--
  - instance {-# OVERLAPPABLE #-} (Functor f, GSet g a) => GSet g (f a) where
  -     act g fa = fmap (act g) fa
  --}

newtype FreeGSet g a = FreeGSet { runFreeGSet :: (g, a) }
    deriving (Show, Ord, Eq, Functor)

instance Group g => Applicative (FreeGSet g) where
    pure  = returnFree
    (<*>) = ap

instance Group g => Monad (FreeGSet g) where
    return = returnFree
    (>>=)  = bindFree

instance Group g => GSet g (FreeGSet g a) where
    act g (FreeGSet (h, a)) = FreeGSet $ (g <> h, a)

type instance AlgebraType (FreeGSet g) a = GSet g a
instance Group g => FreeAlgebra (FreeGSet g) where
    returnFree a = FreeGSet (mempty, a)
    foldMapFree f (FreeGSet (g, a)) = act g (f a)
