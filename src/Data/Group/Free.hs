module Data.Group.Free
    ( FreeGroup
    , fromList
    , toList
    , normalize
    ) where

import           Control.Monad (ap)
import           Data.Group (Group (..))
import           Data.Semigroup (Semigroup (..))

import           Data.Algebra.Free (AlgebraType, FreeAlgebra (..))

-- |
-- Free group generated by type @a@.  Internally it's represented by a list
-- @[Either a a]@ where inverse is given by:
--
-- @
--  inverse (FreeGroup [a]) = FreeGroup [either Right Left a]
-- @
--
-- It is a monad on a full subcategory of @Hask@ which constists of types which
-- satisfy the @'Eq'@ constraint.
newtype FreeGroup a = FreeGroup { runFreeGroup :: [Either a a] }
    deriving (Show, Eq, Ord)

instance Functor FreeGroup where
    fmap f (FreeGroup as) = FreeGroup $ map (either (Left . f) (Right . f)) as

instance Applicative FreeGroup where
    pure  = returnFree
    (<*>) = ap

instance Monad FreeGroup where
    return a = FreeGroup [Right a]
    FreeGroup as >>= f = FreeGroup $ concatMap (runFreeGroup . either f f) as

-- |
-- Normalize a list, i.e. remove adjusten inverses from a word, i.e.
-- @ab⁻¹ba⁻¹c = c@
--
-- Complexity: @O(n)@
normalize
    :: Eq a
    => [Either a a]
    -> [Either a a]

normalize (Left a : Right b : bs)
    | a == b    = normalize bs
    | otherwise = case normalize (Right b : bs) of
        Right b' : bs' | a == b'
                       -> bs'
                       | otherwise
                       -> Left a : Right b' : bs'
        bs'            -> Left a : bs'

normalize (Right a : Left b : bs)
    | a == b    = normalize bs
    | otherwise = case normalize (Left b : bs) of
        Left b' : bs' | a == b'
                      -> bs'
                      | otherwise
                      -> Right a : Left b' : bs'
        bs'           -> Right a : bs'

normalize (a : as) = case normalize as of
    a' : as' | either Right Left a == a'
             -> as'
             | otherwise
             -> a : a' : as'
    []       -> [a]

normalize [] = []

-- |
-- Smart constructor which normalizes a list.
fromList :: Eq a => [Either a a] -> FreeGroup a
fromList = FreeGroup . normalize

toList :: FreeGroup a -> [Either a a]
toList = runFreeGroup


instance Eq a => Semigroup (FreeGroup a) where
    FreeGroup as <> FreeGroup bs = FreeGroup $ normalize (as ++ bs)

instance Eq a => Monoid (FreeGroup a) where
    mempty = FreeGroup []

instance Eq a => Group (FreeGroup a) where
    invert (FreeGroup as) = FreeGroup $ foldl (\acu a -> either Right Left a : acu) [] as

type instance AlgebraType FreeGroup g = Group g
instance FreeAlgebra FreeGroup where
    returnFree a = FreeGroup [Right a]
    foldMapFree _ (FreeGroup [])       = mempty
    foldMapFree f (FreeGroup (a : as)) = either (invert . f) f a <> foldMapFree f (FreeGroup as)
