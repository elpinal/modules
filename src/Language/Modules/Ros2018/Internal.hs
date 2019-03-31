{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}

module Language.Modules.Ros2018.Internal
  (
  -- * Objects
    Variable
  , variable

  -- * Syntax
  , Kind(..)
  , Type(..)
  , BaseType(..)

  -- * Environments
  , Env
  , emptyEnv
  , insertType
  , insertValue
  , lookupType
  , lookupValueByName

  -- * Errors
  , EnvError(..)

  -- * Failure
  , Failure(..)
  , Evidence(..)
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Data.Coerce
import qualified Data.Map.Lazy as Map
import GHC.Generics

import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Shift

newtype Label = Label String
  deriving (Eq, Ord, Show)

newtype Variable = Variable Int
  deriving (Eq, Show)
  deriving Shift via IndexedVariable

variable :: Int -> Variable
variable = coerce

instance Display Variable where
  display (Variable n) = "v[" ++ show n ++ "]"

newtype Generated = Generated Int
  deriving (Eq, Show)

newtype Name = Name String
  deriving (Eq, Ord, Show)

instance Display Name where
  display (Name s) = s

newtype Record a = Record { getRecord :: Map.Map Label a }
  deriving (Eq, Show)
  deriving Functor

instance Shift a => Shift (Record a) where
  shiftAbove c d = fmap $ shiftAbove c d

data Kind
  = Base
  | KFun Kind Kind
  deriving (Eq, Show)
  deriving Shift via Fixed Kind

data BaseType
  = Bool
  | Int
  | Char
  deriving (Eq, Show)
  deriving Shift via Fixed BaseType

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
  deriving Generic

instance Shift Type where
  shiftAbove c d (Forall k ty) = Forall k $ shiftAbove (c + 1) d ty
  shiftAbove c d (Some k ty)   = Some k $ shiftAbove (c + 1) d ty
  shiftAbove c d (TAbs k ty)   = TAbs k $ shiftAbove (c + 1) d ty
  shiftAbove c d ty            = to $ gShiftAbove c d $ from ty

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
  }

emptyEnv :: Env f ty
emptyEnv = Env
  { tenv = []
  , venv = []
  , nmap = mempty
  , tempVenv = []
  }

class Annotated f where
  extract :: f a -> a

data Failure = forall a. Failure a (Evidence a) (a -> String)

data Evidence a where
  EvidEnv :: Evidence EnvError

class Display a => SpecificError a where
  evidence :: Evidence a

fromSpecific :: SpecificError a => a -> Failure
fromSpecific x = Failure x evidence display

throw :: (Member (Error Failure) r, SpecificError a) => a -> Eff r b
throw = throwError . fromSpecific

data EnvError
  = UnboundName Name
  | UnboundVariable Variable
  | UnboundTypeVariable Variable
  deriving (Eq, Show)

instance Display EnvError where
  display (UnboundName name) = "unbound name: " ++ display name
  display (UnboundVariable v) = "unbound variable: " ++ display v
  display (UnboundTypeVariable v) = "unbound type variable: " ++ display v

instance SpecificError EnvError where
  evidence = EvidEnv

insertType :: (Shift ty, ?env :: Env f ty) => f Kind -> Env f ty
insertType k = ?env
  { tenv = k : tenv ?env
  , venv = shift 1 $ venv ?env
  , tempVenv = shift 1 $ tempVenv ?env
  }

insertValue :: (?env :: Env f ty) => Name -> ty -> Env f ty
insertValue name ty = ?env
  { venv = ty : venv ?env
  , nmap = Map.insert name (length (venv ?env) + 1) $ nmap ?env
  }

lookupType :: (Member (Error Failure) r, ?env :: Env f ty) => Variable -> Eff r (f Kind)
lookupType (Variable n) = do
  case tenv ?env of
    xs | 0 <= n && n < length xs -> return $ xs !! n
    _                            -> throw $ UnboundTypeVariable $ Variable n

lookupValueByName :: (Member (Error Failure) r, ?env :: Env f ty) => Name -> Eff r ty
lookupValueByName name = do
  n <- maybe (throw $ UnboundName name) return $ Map.lookup name $ nmap ?env
  lookupValue $ Variable $ length (venv ?env) - n

lookupValue :: (Member (Error Failure) r, ?env :: Env f ty) => Variable -> Eff r ty
lookupValue (Variable n) = do
  case venv ?env of
    xs | 0 <= n && n < length xs -> return $ xs !! n
    _                            -> throw $ UnboundVariable $ Variable n
