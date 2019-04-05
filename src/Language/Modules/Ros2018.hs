{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Modules.Ros2018
  (
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Data.Coerce
import Data.Functor.Identity

import Language.Modules.Ros2018.Display
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal (Term, Literal(..), BaseType, Name, lookupValueByName)
import Language.Modules.Ros2018.Position

type IType = I.Type
type IKind = I.Kind

newtype Ident = Ident Name
  deriving (Eq, Show)

instance Display Ident where
  display (Ident name) = display name

data Binding
  = Val Ident (Positional Expr)
  deriving (Eq, Show)

data Expr
  = Lit Literal
  | Id Ident
  deriving (Eq, Show)

type Env = I.Env Identity LargeType

data LargeType
  = BaseType BaseType
  deriving (Eq, Show)

newtype Quantified a = Quantified ([Positional IKind], a)
  deriving (Eq, Show)

newtype Existential a = Existential (Quantified a)
  deriving (Eq, Show)

newtype Universal a = Universal (Quantified a)
  deriving (Eq, Show)

type AbstractType = Existential LargeType

data Purity
  = Pure
  | Impure
  deriving (Eq, Show)

class Elaboration a where
  type Output a
  type Effs a :: [* -> *]

  elaborate :: (Members (Effs a) r, ?env :: Env) => Positional a -> Eff r (Output a)

instance Elaboration Literal where
  type Output Literal = BaseType
  type Effs Literal = '[]

  elaborate = return . I.typeOfLiteral . fromPositional

pattern WoPos :: a -> Positional a
pattern WoPos x <- Positional _ x

instance Elaboration Expr where
  type Output Expr = (Term, LargeType, Purity)
  type Effs Expr = '[Error I.Failure]

  elaborate (Positional pos (Lit l)) = do
    b <- elaborate $ Positional pos l
    return (I.Lit l, BaseType b, Pure) -- Literals are always pure.
  elaborate (WoPos (Id id)) = do
    (lty, v) <- lookupValueByName $ coerce id
    return (I.Var v, lty, Pure)
