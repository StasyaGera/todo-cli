module Data.NESet
    ( NonEmptySet (..)
    , singleton
    , Data.NESet.filter
    , member
    , fromList
    , fromSet
    , delete
    ) where

import Data.HashSet (HashSet)
import GHC.Generics (Generic)
import qualified Data.HashSet as HSet
import Data.Hashable (Hashable (..))
import Data.Foldable (toList)

data NonEmptySet a = a :< HashSet a 
    deriving (Eq, Show, Generic, Foldable)

instance Hashable a => Hashable (NonEmptySet a)

singleton :: (Hashable a, Eq a) => a -> NonEmptySet a
singleton x = x :< mempty

filter :: (Hashable a, Ord a) => (a -> Bool) -> NonEmptySet a -> HashSet a
filter p (x :< xs) = if p x then HSet.insert x pxs else pxs where pxs = HSet.filter p xs

member :: (Hashable a, Ord a) => a -> NonEmptySet a -> Bool
member y (x :< xs) = x == y || HSet.member y xs

fromList :: (Hashable a, Ord a) => [a] -> Maybe (NonEmptySet a)
fromList []       = Nothing
fromList (x : xs) = Just $ x :< HSet.fromList xs

fromSet :: (Hashable a, Eq a) => HashSet a -> Maybe (NonEmptySet a)
fromSet xs 
    | HSet.null xs = Nothing
    | otherwise    = Just $ y :< HSet.fromList ys where (y : ys) = toList xs

delete :: (Hashable a, Ord a) => a -> NonEmptySet a -> HashSet a
delete y (x :< xs) 
    | x == y    = xs 
    | otherwise = HSet.insert x $ HSet.delete y xs
