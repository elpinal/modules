{-|
Module  : Language.Modules.DCH2003
License : MIT

Derek Dreyer, Karl Crary, and Robert Harper.
A type system for higher-order modules.
In Thirtieth ACM Symposium on Principles of Programming Languages, pages 236–249, New Orleans, Louisiana, January 2003.
-}

module Language.Modules.DCH2003
  (
  ) where

data Term
  = ProjTerm Module
  | Pair Term Term
  | Fst Term
  | Snd Term
  | App Term Module
  | Fix Signature Type Term
  | Let Module Term Type
  deriving (Eq, Show)

data Type
  = Int
  | ProjType Module
  -- | Dependent product.
  | DProd Signature Type
  | Prod Type Type
  deriving (Eq, Show)

data Module
  = Var Int
  | Unit
  | AtomicType Type
  | AtomicTerm Term Type
  -- | Module abstraction.
  | MAbs Signature Module
  -- | Module application.
  | MApp Module Module
  -- | A pair whose second component may depend on the first component.
  | DPair Module Module
  | MFst Module
  | MSnd Module
  | MLet Module Module Signature
  -- | Strong sealing.
  | Module ::> Signature
  -- | Weak sealing.
  | Module ::: Signature
  deriving (Eq, Show)

data Signature
  = UnitSig
  | AtomicTypeSig
  | AtomicTermSig Type
  -- | Total dependent functor.
  | Total Signature Signature
  -- | Partial dependent functor.
  | Partial Signature Signature
  -- | Dependent sum.
  | DSum Signature Signature
  | Singleton Module
  deriving (Eq, Show)
