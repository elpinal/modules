{-# LANGUAGE DeriveTraversable #-}

module Language.Modules.RRD2010
  (
  ) where

newtype Ident = Ident Int
  deriving (Eq, Show)

data Kind = Mono
  deriving (Eq, Show)

data Type
  = Int
  deriving (Eq, Show)

data Expr
  = IntLit Int
  deriving (Eq, Show)

newtype Path = Path Module
  deriving (Eq, Show)

data Module
  = ModleIdent Ident
  | Bindings [Binding]
  | Projection Module Ident
  | Fun Ident Sig Module
  | ModuleApp Ident Ident
  | Ident :> Sig
  deriving (Eq, Show)

data Binding
  = Val Ident Expr
  | Type Ident Type
  | Module Ident Module
  | Signature Ident Sig
  | Include Module
  deriving (Eq, Show)

data Sig
  = SigPath Path
  | Decls [Decl]
  | FunSig Ident Sig Sig
  | Where Sig (Proj Ident) Type
  deriving (Eq, Show)

data Proj a = Proj a [a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

data Decl
  = ValDecl Ident Type
  | ManTypeDecl Ident Type
  | AbsTypeDecl Ident Kind
  | ModuleDecl Ident Sig
  | SignatureDecl Ident Sig
  | IncludeDecl Sig
  deriving (Eq, Show)
