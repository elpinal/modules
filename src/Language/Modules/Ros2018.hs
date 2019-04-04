{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Modules.Ros2018
  (
  ) where

import Control.Monad.Freer

import Language.Modules.Ros2018.Display
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal (Term, Name)

type IType = I.Type
type ILiteral = I.Literal

newtype Ident = Ident Name
  deriving (Eq, Show)

instance Display Ident where
  display (Ident name) = display name

data Binding
  = Val Ident Expr
  deriving (Eq, Show)

data Literal
  = LBool Bool
  deriving (Eq, Show)

data Expr
  = Id Ident
  | Lit Literal
  deriving (Eq, Show)

class Elaboration a where
  type Output a
  type Effs a :: [* -> *]

  elaborate :: Members (Effs a) r => a -> Eff r (Output a)

instance Elaboration Literal where
  type Output Literal = (ILiteral, I.BaseType)
  type Effs Literal = '[]

  elaborate (LBool b) = return (I.LBool b, I.Bool)
