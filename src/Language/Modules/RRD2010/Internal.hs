{-# LANGUAGE DeriveTraversable #-}

module Language.Modules.RRD2010.Internal
  ( Label
  , Variable
  , Record(..)
  , Kind(..)
  , Type(..)
  , Term(..)

  -- * Environment
  , Env
  , lookupKind
  , lookupType
  ) where

import qualified Data.Map.Lazy as Map

data Label
  = Label Int
  | Val
  | Typ
  | Sig
  deriving (Eq, Ord, Show)

newtype Variable = Variable Int
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
