module Language.Modules.Ros2018.Internal
  (
  ) where

import qualified Data.Map.Lazy as Map

newtype Label = Label String
  deriving (Eq, Ord, Show)

newtype Variable = Variable Int
  deriving (Eq, Show)

newtype Generated = Generated Int
  deriving (Eq, Show)

newtype Name = Name String
  deriving (Eq, Show)

newtype Record a = Record { getRecord :: Map.Map Label a }
  deriving (Eq, Show)

data Kind
  = Base
  | KFun Kind Kind
  deriving (Eq, Show)

data BaseType
  = Bool
  | Int
  | Char
  deriving (Eq, Show)

data Type
  = BaseType BaseType
  | TVar Variable
  | TFun Type Type
  | TRecord (Record Type)
  | Forall Kind Type
  | Some Kind Type
  | TAbs Kind Type
  | TApp Type Type
  deriving (Eq, Show)

data Term
  = Var Variable
  | Abs Type Term
  | App Term Term
  | TmRecord (Record Term)
  | Proj Term Label
  | Poly Kind Term
  | Inst Term Type
  | Pack Term [Type] [Kind] Type
  | Unpack (Maybe Generated) Term Int Term
  -- To make debug easy.
  | Let Term Term
  deriving (Eq, Show)

data Env f ty = Env
  { tenv :: [f Kind]
  , venv :: [ty]
  , nmap :: Map.Map Name Int
  , tempVenv :: [ty]
  , count :: Int
  }

class Annotated f where
  extract :: f a -> a
