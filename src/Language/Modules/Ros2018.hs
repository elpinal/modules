{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Modules.Ros2018
  (
  -- * Objects
    Ident
  , ident

  -- * Syntax
  , Expr(..)
  , Binding(..)

  -- * Environments
  , Env

  -- * Elaboration
  , Elaboration(..)
  , translate

  -- * Purity
  , Purity(..)

  -- * Semantic objects
  , LargeType(..)

  -- * Quantification
  , Existential
  , Universal
  , Quantification(..)
  ) where

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Error
import Data.Coerce
import Data.Functor.Identity
import Data.List
import Data.Monoid
import qualified Data.Text as T

import Language.Modules.Ros2018.Display
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal hiding (Env, Term(..), Type(..), Kind(..))
import Language.Modules.Ros2018.Internal (Term)
import Language.Modules.Ros2018.Position

type IType = I.Type
type IKind = I.Kind

newtype Ident = Ident Name
  deriving (Eq, Show)

ident :: T.Text -> Ident
ident = Ident . name

instance Display Ident where
  display (Ident name) = display name

data Binding
  = Val Ident (Positional Expr)
  deriving (Eq, Show)

instance Display Binding where
  display (Val id e) = display id ++ " = " ++ display (fromPositional e)

data Expr
  = Lit Literal
  | Id Ident
  deriving (Eq, Show)

instance Display Expr where
  display (Lit l) = display l
  display (Id id) = display id

type Env = I.Env Identity LargeType

data LargeType
  = BaseType BaseType
  | Structure (Record LargeType)
  deriving (Eq, Show)

instance DisplayName LargeType where
  displaysWithName _ (BaseType b)  = displays b
  displaysWithName _ (Structure r) = displaysWithName 0 r

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
  fromBody :: a -> f a

instance Quantification Quantified where
  getKinds (Quantified (ks, _)) = map fromPositional ks
  qsLen (Quantified (ks, _)) = length ks
  enumVars q = map variable [0..qsLen q-1]
  getBody (Quantified (_, x)) = x
  fromBody x = Quantified ([], x)

newtype Existential a = Existential (Quantified a)
  deriving (Eq, Show)
  deriving Functor
  deriving Quantification

instance DisplayName a => DisplayName (Existential a) where
  displaysWithName _ (Existential (Quantified (ks, x))) =
    let ?nctx = newTypes $ length ks in
    let f = mconcat $ coerce $ intersperse (showString ", ") $ map (\(i, k) -> displayTypeVariable i . showString " : " . displays k) $ zip [0..] ks in
    showString "∃" . appEndo f . showString ". " . displaysWithName 0 x

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

instance Display Purity where
  display Pure   = "pure"
  display Impure = "impure"

class ToType a where
  toType :: a -> IType

instance ToType LargeType where
  toType (BaseType b)  = I.BaseType b
  toType (Structure r) = I.TRecord $ toType <$> r

translate :: Positional Binding -> Either I.Failure (Term, AbstractType, Purity)
translate b = run $ runError $ let ?env = I.emptyEnv in elaborate b

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
    return (I.unpack Nothing t (qsLen aty) $ I.pack (I.TmRecord $ record [(l, var 0)]) (I.TVar <$> enumVars aty) (getKinds aty) $ toType $ getBody aty, (\x -> Structure $ record [(l, x)]) <$> aty, p)
