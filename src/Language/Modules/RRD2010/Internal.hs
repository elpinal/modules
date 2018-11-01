{-# LANGUAGE DeriveTraversable #-}

module Language.Modules.RRD2010.Internal
  ( Label(..)
  , Variable(..)
  , Record(..)
  , Kind(..)
  , Type(..)
  , some
  , forall
  , Term(..)

  -- * Environment
  , Env
  , lookupKind
  , lookupType
  , insertKind
  , insertType
  ) where

import qualified Data.Map.Lazy as Map

data Label
  = Label Int
  | Val
  | Typ
  | Sig
  deriving (Eq, Ord, Show)

newtype Variable = Variable Int
  deriving (Eq, Ord, Show)

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
  | Forall Variable Kind Type
  | Some Variable Kind Type
  | TAbs Variable Kind Type
  | TApp Type Type
  | Int
  deriving (Eq, Show)

some :: Map.Map Variable Kind -> Type -> Type
some m ty = Map.foldrWithKey Some ty m

forall :: Map.Map Variable Kind -> Type -> Type
forall m ty = Map.foldrWithKey Forall ty m

data Term
  = Var Variable
  | Abs Variable Type Term
  | App Term Term
  | TmRecord (Record Term)
  | Proj Term Label
  | Poly Variable Kind Term
  | Inst Term Type
  | Pack Type Term Type
  | Unpack Variable Variable Term Term
  deriving (Eq, Show)

newtype Env = Env [(Variable, Either Kind Type)]
  deriving (Eq, Show)

lookupEnv :: Variable -> Env -> Maybe (Either Kind Type)
lookupEnv v (Env xs) = lookup v xs

lookupKind :: Variable -> Env -> Maybe Kind
lookupKind v e = lookupEnv v e >>= either Just (const Nothing)

lookupType :: Variable -> Env -> Maybe Type
lookupType v e = lookupEnv v e >>= either (const Nothing) Just

insertKind :: Variable -> Kind -> Env -> Env
insertKind v k (Env e) = Env $ (v, Left k) : e

insertType :: Variable -> Type -> Env -> Env
insertType v k (Env e) = Env $ (v, Right k) : e
