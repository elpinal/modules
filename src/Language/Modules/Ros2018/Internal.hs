{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module Language.Modules.Ros2018.Internal
  (
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import qualified Data.Map.Lazy as Map

import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Shift

newtype Label = Label String
  deriving (Eq, Ord, Show)

newtype Variable = Variable Int
  deriving (Eq, Show)

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
  }

class Annotated f where
  extract :: f a -> a

data Failure = forall a. Failure a (Maybe (Evidence a)) (a -> String)

data Evidence a where
  EvidEnv :: Evidence EnvError

class Display a => SpecificError a where
  evidence :: Evidence a

fromDisplay :: Display a => a -> Failure
fromDisplay x = Failure x Nothing display

fromSpecific :: SpecificError a => a -> Failure
fromSpecific x = Failure x (Just evidence) display

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
