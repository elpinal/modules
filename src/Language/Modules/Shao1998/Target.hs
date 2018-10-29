{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Modules.Shao1998.Target
  ( TyCon(..)
  , Type(..)
  , Kind(..)
  , KindEnv(..)
  , Decl(..)
  , Term(..)
  , Label(..)
  ) where

import qualified Data.Map.Lazy as Map

newtype Label = Label Int
  deriving (Eq, Ord, Show)

data Kind
  = Mono
  | KFun Kind Kind
  | KProduct (Map.Map Label Kind)
  deriving (Eq, Show)

data TyCon
  = TyConVar Int
  | TyConInt
  | TyConFun TyCon TyCon
  | TyConAbs Kind TyCon
  | TyConApp TyCon TyCon
  | TyConProduct (Map.Map Label TyCon)
  | TyConSelect TyCon Label
  deriving (Eq, Show)

data Type
  = Type TyCon
  | TyFun Type Type
  | TyProduct (Map.Map Label Type)
  | Forall Kind Type
  deriving (Eq, Show)

data Term
  = Var Int
  | Int Int
  | Abs Type Term
  | App Term Term
  | TyAbs Kind Term
  | TyApp Term TyCon
  | Product (Map.Map Label Term)
  | Select Term Label
  | Let Decl Term
  deriving (Eq, Show)

newtype Decl = Decl [Term]
  deriving (Eq, Show, Semigroup, Monoid)

newtype KindEnv = KindEnv [Kind]
  deriving (Eq, Show)
