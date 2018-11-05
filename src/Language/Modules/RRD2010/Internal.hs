{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Modules.RRD2010.Internal
  ( Name(..)
  , Label(..)
  , Variable(..)
  , Record(..)
  , Kind(..)
  , Type(..)
  , some
  , forall
  , Term(..)
  , poly
  , inst

  , Shift(..)
  , shift

  -- * Environment
  , Env
  , VarInfo(..)
  , getName
  , lookupName
  , lookupKind
  , lookupType
  , insertKind
  , insertType
  ) where

import Data.Coerce
import qualified Data.Map.Lazy as Map

newtype Name = Name String
  deriving (Eq, Ord, Show)

data Label
  = Label Name
  | Val
  | Typ
  | Sig
  deriving (Eq, Ord, Show)

newtype Variable = Variable Int
  deriving (Eq, Ord, Show)

add :: Variable -> Int -> Variable
add (Variable m) n = Variable $ m + n

sub :: Variable -> Int -> Variable
sub (Variable m) n = Variable $ m - n

newtype Record a = Record (Map.Map Label a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Kind
  = Mono
  | KFun Kind Kind
  deriving (Eq, Show)

data Type
  = TVar Variable
  | TFun Type Type
  | TRecord (Record Type)
  | Forall Kind Type
  | Some Kind Type
  | TAbs Kind Type
  | TApp Type Type
  | Int
  deriving (Eq, Show)

some :: [Kind] -> Type -> Type
some ks ty = foldl (flip Some) ty ks

forall :: [Kind] -> Type -> Type
forall ks ty = foldl (flip Forall) ty ks

data Term
  = Var Variable
  | Abs Type Term
  | App Term Term
  | TmRecord (Record Term)
  | Proj Term Label
  | Poly Kind Term
  | Inst Term Type
  | Pack Type Term Type
  | Unpack Term Term
  deriving (Eq, Show)

poly :: [Kind] -> Term -> Term
poly ks t = foldr Poly t ks

inst :: Term -> [Type] -> Term
inst t ts = foldr (flip Inst) t ts

data VarInfo a = VarInfo Name a
  deriving (Eq, Show, Functor)

getName :: VarInfo a -> Name
getName (VarInfo name _) = name

data Env a = Env
  { tenv :: [VarInfo a]
  , kenv :: [Kind]
  }
  deriving (Eq, Show)

lookupKind :: Variable -> Env a -> Maybe Kind
lookupKind (Variable n) (kenv -> xs)
  | 0 <= n && n < length xs = return $ xs !! n
  | otherwise               = Nothing

lookupType :: Variable -> Env a -> Maybe (VarInfo a)
lookupType (Variable n) (tenv -> xs)
  | 0 <= n && n < length xs = return $ xs !! n
  | otherwise               = Nothing

lookupName :: Variable -> Env a -> Maybe Name
lookupName v e = getName <$> lookupType v e

insertKind :: Shift a => Kind -> Env a -> Env a
insertKind k e = shift 1 $ e { kenv = k : kenv e }

insertType :: Name -> a -> Env a -> Env a
insertType name x e = e { tenv = VarInfo name x : tenv e }

shift :: Shift a => Int -> a -> a
shift = shiftAbove 0

class Shift a where
  shiftAbove :: Int -> Int -> a -> a

instance Shift Type where
  shiftAbove c0 d = walk c0
    where
      walk :: Int -> Type -> Type
      walk c t @ (TVar v)
        | c <= coerce v = TVar $ add v d
        | otherwise     = t
      walk c (TFun t1 t2) = TFun (walk c t1) (walk c t2)
      walk c (TRecord r)  = TRecord $ walk c <$> r
      walk c (Forall k t) = Forall k $ walk (c + 1) t
      walk c (Some k t)   = Some k $ walk (c + 1) t
      walk c (TAbs k t)   = TAbs k $ walk (c + 1) t
      walk c (TApp t1 t2) = TApp (walk c t1) (walk c t2)
      walk _ Int          = Int

instance Shift a => Shift (Env a) where
  shiftAbove c0 d e = e { tenv = shiftAbove c0 d <$> tenv e }

instance Shift a => Shift (VarInfo a) where
  shiftAbove c0 d info = shiftAbove c0 d <$> info

instance Shift a => Shift (Map.Map k a) where
  shiftAbove c0 d m = shiftAbove c0 d <$> m
