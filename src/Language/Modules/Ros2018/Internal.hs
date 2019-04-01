{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Modules.Ros2018.Internal
  (
  -- * Objects
    Variable
  , variable
  , Name
  , name
  , Label
  , label

  -- * Records
  , Record
  , record

  -- * Syntax
  , Kind(..)
  , Type(..)
  , BaseType(..)
  , Term(..)

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
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Monoid
import GHC.Generics

import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Shift

newtype Label = Label String
  deriving (Eq, Ord, Show)

label :: String -> Label
label = coerce

instance Display Label where
  display (Label s) = s

newtype Variable = Variable { getVariable :: Int }
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

name :: String -> Name
name = coerce

instance Display Name where
  display (Name s) = s

newtype Record a = Record { getRecord :: Map.Map Label a }
  deriving (Eq, Show)
  deriving Functor

instance Display a => Display (Record a) where
  displaysPrec _ (Record m) =
    let xs = map (\(l, x) -> displays l . showString ": " . displays x) $ Map.toList m in
    let ys = intersperse (showString ", ") xs in
      showString "{" . appEndo (mconcat $ coerce ys) . showString "}"

instance DisplayName a => DisplayName (Record a) where
  displaysWithName _ (Record m) =
    let xs = map (\(l, x) -> displays l . showString ": " . displaysWithName 0 x) $ Map.toList m in
    let ys = intersperse (showString ", ") xs in
      showString "{" . appEndo (mconcat $ coerce ys) . showString "}"

instance Shift a => Shift (Record a) where
  shiftAbove c d = fmap $ shiftAbove c d

record :: [(Label, a)] -> Record a
record = Record . Map.fromList

data Kind
  = Base
  | KFun Kind Kind
  deriving (Eq, Show)
  deriving Shift via Fixed Kind

instance Display Kind where
  displaysPrec _ Base         = showString "*"
  displaysPrec n (KFun k1 k2) = showParen (4 <= n) $ displaysPrec 4 k1 . showString " -> " . displaysPrec 3 k2

data BaseType
  = Bool
  | Int
  | Char
  deriving (Eq, Show)
  deriving Shift via Fixed BaseType

instance Display BaseType where
  display Bool = "bool"
  display Int  = "int"
  display Char = "char"

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

instance Display Type where
  displaysPrec n (BaseType b)   = displaysPrec n b
  displaysPrec n (TVar v)       = displaysPrec n v
  displaysPrec n (TFun ty1 ty2) = showParen (4 <= n) $ displaysPrec 4 ty1 . showString " -> " . displaysPrec 3 ty2
  displaysPrec n (TRecord r)    = displaysPrec n r
  displaysPrec n (Forall k ty)  = showParen (4 <= n) $ showChar '∀' . displays k . showString ". " . displays ty
  displaysPrec n (Some k ty)    = showParen (4 <= n) $ showChar '∃' . displays k . showString ". " . displays ty
  displaysPrec n (TAbs k ty)    = showParen (4 <= n) $ showChar 'λ' . displays k . showString ". " . displays ty
  displaysPrec n (TApp ty1 ty2) = showParen (5 <= n) $ displaysPrec 4 ty1 . showString " " . displaysPrec 5 ty2

instance DisplayName Type where
  displaysWithName n (BaseType b)   = displaysPrec n b
  displaysWithName _ (TVar v)       = displayTypeVariable $ getVariable v
  displaysWithName n (TFun ty1 ty2) = showParen (4 <= n) $ displaysWithName 4 ty1 . showString " -> " . displaysWithName 3 ty2
  displaysWithName n (TRecord r)    = displaysWithName n r
  displaysWithName n (Forall k ty)  =
    let ?nctx = newType in
      showParen (4 <= n) $ showChar '∀' . displayTypeVariable 0 . showString " : " . displays k . showString ". " . displaysWithName 0 ty
  displaysWithName n (Some k ty)    =
    let ?nctx = newType in
      showParen (4 <= n) $ showChar '∃' . displayTypeVariable 0 . showString " : " . displays k . showString ". " . displaysWithName 0 ty
  displaysWithName n (TAbs k ty)    =
    let ?nctx = newType in
      showParen (4 <= n) $ showChar 'λ' . displayTypeVariable 0 . showString " : " . displays k . showString ". " . displaysWithName 0 ty
  displaysWithName n (TApp ty1 ty2) = showParen (5 <= n) $ displaysWithName 4 ty1 . showString " " . displaysWithName 5 ty2

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

instance Display Term where
  displaysPrec n (Var v)       = displaysPrec n v
  displaysPrec n (Abs ty t)    = showParen (4 <= n) $ showChar 'λ' . displays ty . showString ". " . displays t
  displaysPrec n (App t1 t2)   = showParen (5 <= n) $ displaysPrec 4 t1 . showString " " . displaysPrec 5 t2
  displaysPrec n (TmRecord r)  = displaysPrec n r
  displaysPrec _ (Proj t l)    = displaysPrec 5 t . showChar '.' . displays l
  displaysPrec n (Poly k t)    = showParen (4 <= n) $ showChar 'Λ' . displays k . showString ". " . displays t
  displaysPrec n (Inst t ty)   = showParen (5 <= n) $ displaysPrec 4 t . showString " [" . displays ty . showChar ']'

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
  display (UnboundName name)      = "unbound name: " ++ display name
  display (UnboundVariable v)     = "unbound variable: " ++ display v
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