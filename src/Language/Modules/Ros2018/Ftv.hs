{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Modules.Ros2018.Ftv
  ( Ftv(..)
  , GFtv(..)
  , Variable
  , Empty(..)
  ) where

import Data.Proxy
import qualified Data.Set as Set
import GHC.Generics

type family Variable a :: *

-- Free type variables.
class Ftv v a where
  ftv :: Proxy v -> a -> Set.Set (Variable v)

  default ftv :: (Ord (Variable v), Generic a, GFtv v (Rep a)) => Proxy v -> a -> Set.Set (Variable v)
  ftv p x = gFtv p $ from x

class GFtv v f where
  gFtv :: Ord (Variable v) => Proxy v -> f a -> Set.Set (Variable v)

instance GFtv v V1 where
  gFtv _ _ = undefined

instance GFtv v U1 where
  gFtv _ U1 = Set.empty

instance (GFtv v a, GFtv v b) => GFtv v (a :*: b) where
  gFtv p (x :*: y) = gFtv p x <> gFtv p y

instance (GFtv v a, GFtv v b) => GFtv v (a :+: b) where
  gFtv p (L1 x) = gFtv p x
  gFtv p (R1 x) = gFtv p x

instance GFtv v a => GFtv v (M1 i c a) where
  gFtv p (M1 x) = gFtv p x

instance Ftv v a => GFtv v (K1 i a) where
  gFtv p (K1 x) = ftv p x

newtype Empty a = Empty a

instance Ftv v (Empty a) where
  ftv _ _ = Set.empty
