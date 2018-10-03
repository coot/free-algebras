{-# LANGUAGE CPP #-}
{- |
   Free groups

     * https://en.wikipedia.org/wiki/Free_group
     * https://ncatlab.org/nlab/show/Nielsen-Schreier+theorem

 -}
module Data.Group.Free
    ( FreeGroup
    , fromDList
    , toDList
    , normalize

    , FreeGroupL
    , fromList
    , toList
    , normalizeL
    ) where

import           Control.Monad (ap, join)
import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.Group (Group (..))
import           Data.Semigroup (Semigroup (..))

import           Data.Algebra.Free
    ( AlgebraType
    , AlgebraType0
    , FreeAlgebra (..)
    , proof
    )

-- |
-- Free group generated by a type @a@.  Internally it's represented by a list
-- @[Either a a]@ where inverse is given by:
--
-- @
--  inverse (FreeGroup [a]) = FreeGroup [either Right Left a]
-- @
--
-- It is a monad on a full subcategory of @Hask@ which constists of types which
-- satisfy the @'Eq'@ constraint.
--
-- @'FreeGroup' a@ is isomorphic with @'Free' Group a@ (but the latter does not
-- require @Eq@ constraint, hence is more general).
newtype FreeGroup a = FreeGroup { runFreeGroup :: DList (Either a a) }
    deriving (Eq, Ord, Show)

instance Functor FreeGroup where
    fmap f (FreeGroup as) = FreeGroup $ fmap (either (Left . f) (Right . f)) as

instance Applicative FreeGroup where
    pure  = returnFree
    (<*>) = ap

instance Monad FreeGroup where
    return a = FreeGroup $ DList.singleton (Right a)
    FreeGroup as >>= f = FreeGroup $ join $ fmap (runFreeGroup . either f f) as

-- |
-- Normalize a list, i.e. remove adjusten inverses from a word, i.e.
-- @ab⁻¹ba⁻¹c = c@
--
-- Complexity: @O(n)@
normalize
    :: Eq a
    => DList (Either a a)
    -> DList (Either a a)
normalize = DList.foldr fn (DList.empty)
    where
    fn a as = case as of
        DList.Nil -> DList.singleton a
        _         ->
            let b  = DList.head as
                bs = DList.tail as
            in case (a, b) of
                (Left x,  Right y) | x == y -> bs
                (Right x, Left y)  | x == y -> bs
                _                           -> DList.cons a as

-- |
-- Smart constructor which normalizes a list.
fromDList :: Eq a => DList (Either a a) -> FreeGroup a
fromDList = FreeGroup . normalize

toDList :: FreeGroup a -> DList (Either a a)
toDList = runFreeGroup

instance Eq a => Semigroup (FreeGroup a) where
    FreeGroup as <> FreeGroup bs = FreeGroup $ normalize (as `DList.append` bs)

instance Eq a => Monoid (FreeGroup a) where
    mempty = FreeGroup DList.empty
#if __GLASGOW_HASKELL__ <= 822
    mappend = (<>)
#endif

instance Eq a => Group (FreeGroup a) where
    invert (FreeGroup as) = FreeGroup $ foldl (\acu a -> either Right Left a `DList.cons` acu) DList.empty as

type instance AlgebraType0 FreeGroup a = Eq a
type instance AlgebraType  FreeGroup g = (Eq g, Group g)
instance FreeAlgebra FreeGroup where
    returnFree a = FreeGroup (DList.singleton (Right a))
    foldMapFree _ (FreeGroup DList.Nil) = mempty
    foldMapFree f (FreeGroup as)        =
        let a'  = DList.head as
            as' = DList.tail as
        in either (invert . f) f a' `mappend` foldMapFree f (FreeGroup as')

    codom  = proof
    forget = proof

-- |
-- Free group in the class of groups which multiplication is strict on the left, i.e.
--
-- prop> undefined <> a = undefined
newtype FreeGroupL a = FreeGroupL { runFreeGroupL :: [Either a a] }
    deriving (Show, Eq, Ord)

normalizeL
    :: Eq a
    => [Either a a]
    -> [Either a a]
normalizeL = DList.toList . normalize . DList.fromList

-- |
-- Smart constructors
fromList :: Eq a => [Either a a] -> FreeGroupL a
fromList = FreeGroupL . normalizeL

toList :: FreeGroupL a -> [Either a a]
toList = runFreeGroupL

instance Eq a => Semigroup (FreeGroupL a) where
    FreeGroupL as <> FreeGroupL bs = FreeGroupL $ normalizeL (as ++ bs)

instance Eq a => Monoid (FreeGroupL a) where
    mempty = FreeGroupL []
#if __GLASGOW_HASKELL__ <= 822
    mappend = (<>)
#endif

instance Eq a => Group (FreeGroupL a) where
    invert (FreeGroupL as) = FreeGroupL $ foldl (\acu a -> either Right Left a : acu) [] as

type instance AlgebraType0 FreeGroupL a = Eq a
type instance AlgebraType  FreeGroupL g = (Eq g, Group g)
instance FreeAlgebra FreeGroupL where
    returnFree a = FreeGroupL [Right a]
    foldMapFree _ (FreeGroupL []) = mempty
    foldMapFree f (FreeGroupL (a : as)) =
        either (invert . f) f a `mappend` foldMapFree f (FreeGroupL as)

    codom  = proof
    forget = proof
