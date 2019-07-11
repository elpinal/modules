{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Modules.Ros2018.NDList
  ( NDList
  , empty
  , (<&)
  ) where

import Data.Coerce
import qualified Data.Set as Set
import GHC.Exts

-- A list with no duplicates.
newtype NDList a = NDList [a]
  deriving (Eq, Show, Foldable)

type role NDList nominal

instance Ord a => IsList (NDList a) where
  type Item (NDList a) = a

  toList   = coerce
  fromList = coerce . go mempty
    where
      go _ []       = []
      go s (x : xs)
        | x `Set.member` s = error "fromList: duplicate elements" -- report an error dynamically
        | otherwise        = x : go (Set.insert x s) xs

empty :: NDList a
empty = NDList []

(<&) :: Ord a => a -> NDList a -> Maybe (NDList a)
x <& (NDList xs)
  | x `Set.member` Set.fromList xs = Nothing
  | otherwise                      = Just $ NDList $ x : xs
