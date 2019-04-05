{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
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
import Language.Modules.Ros2018.Internal hiding (Env, Term(..), Type(..), Kind(..))
import Language.Modules.Ros2018.Internal (Term)
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
  | Structure (Record LargeType)
  deriving (Eq, Show)

newtype Quantified a = Quantified ([Positional IKind], a)
  deriving (Eq, Show)
  deriving Functor

toQuantified :: a -> Quantified a
toQuantified x = Quantified ([], x)

class Quantification f where
  getKinds :: f a -> [IKind]
  qsLen :: f a -> Int
  enumVars :: f a -> [Variable]
  getBody :: f a -> a

instance Quantification Quantified where
  getKinds (Quantified (ks, _)) = map fromPositional ks
  qsLen (Quantified (ks, _)) = length ks
  enumVars q = map variable [0..qsLen q]
  getBody (Quantified (_, x)) = x

newtype Existential a = Existential (Quantified a)
  deriving (Eq, Show)
  deriving Functor
  deriving Quantification

toExistential :: a -> Existential a
toExistential = Existential . toQuantified

newtype Universal a = Universal (Quantified a)
  deriving (Eq, Show)
  deriving Functor
  deriving Quantification

toUniversal :: a -> Universal a
toUniversal = Universal . toQuantified

type AbstractType = Existential LargeType

data Purity
  = Pure
  | Impure
  deriving (Eq, Show)

class ToType a where
  toType :: a -> IType

instance ToType LargeType where
  toType (BaseType b)  = I.BaseType b
  toType (Structure r) = I.TRecord $ toType <$> r

class Elaboration a where
  type Output a
  type Effs a :: [* -> *]

  elaborate :: (Members (Effs a) r, ?env :: Env) => Positional a -> Eff r (Output a)

instance Elaboration Literal where
  type Output Literal = BaseType
  type Effs Literal = '[]

  elaborate = return . I.typeOfLiteral . fromPositional

instance Elaboration Expr where
  type Output Expr = (Term, AbstractType, Purity)
  type Effs Expr = '[Error I.Failure]

  elaborate (Positional pos (Lit l)) = do
    b <- elaborate $ Positional pos l
    return (I.Lit l, toExistential $ BaseType b, Pure) -- Literals are always pure.
  elaborate (Positional _ (Id id)) = do
    (lty, v) <- lookupValueByName $ coerce id
    return (I.Var v, toExistential lty, Pure)

instance Elaboration Binding where
  type Output Binding = (Term, AbstractType, Purity)
  type Effs Binding = '[Error I.Failure]

  elaborate (Positional _ (Val id e)) = do
    (t, aty, p) <- elaborate e
    let l = I.toLabel $ coerce id
    return (I.Unpack Nothing t (qsLen aty) $ I.Pack (I.TmRecord $ record [(l, var 0)]) (I.TVar <$> enumVars aty) (getKinds aty) $ toType $ getBody aty, (\x -> Structure $ record [(l, x)]) <$> aty, p)
