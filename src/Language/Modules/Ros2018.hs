{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Modules.Ros2018
  (
  -- * Objects
    Ident
  , ident

  -- * Syntax
  , Type(..)
  , Expr(..)
  , Binding(..)

  -- * Environments
  , Env

  -- * Elaboration
  , Elaboration(..)
  , translate
  , runElaborate

  -- * Errors
  , ElaborateError

  -- * Purity
  , Purity(..)

  -- * Semantic objects
  , LargeType(..)
  , AbstractType

  -- * Embedding to internal objects
  , ToType(..)

  -- * Quantification
  , Existential
  , Universal
  , Quantification(..)
  ) where

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Error
import Control.Monad.Freer.Fresh
import Data.Coerce
import Data.Foldable
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Monoid
import qualified Data.Text as T
import GHC.Generics

import Language.Modules.Ros2018.Display
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal hiding (Env, Term(..), Type(..), Kind(..))
import Language.Modules.Ros2018.Internal (Term)
import Language.Modules.Ros2018.Position
import Language.Modules.Ros2018.Shift

type IType = I.Type
type IKind = I.Kind

newtype Ident = Ident Name
  deriving (Eq, Show)

ident :: T.Text -> Ident
ident = Ident . name

instance Display Ident where
  display (Ident name) = display name

data Type
  = Base BaseType
  | TypeType
  deriving (Eq, Show)

instance Display Type where
  displaysPrec _ (Base b) = displays b
  displaysPrec _ TypeType = showString "type"

data Binding
  = Val Ident (Positional Expr)
  | Include (Positional Expr)
  deriving (Eq, Show)

instance Display Binding where
  displaysPrec _ (Val id e)  = displays id . showString " = " . displays (fromPositional e)
  displaysPrec _ (Include e) = showString "include " . displaysPrec 5 (fromPositional e)

data Expr
  = Lit Literal
  | Id Ident
  | Struct [Positional Binding]
  | Type (Positional Type)
  deriving (Eq, Show)

instance Display Expr where
  displaysPrec _ (Lit l)     = displays l
  displaysPrec _ (Id id)     = displays id
  displaysPrec _ (Struct bs) = showString "struct " . appEndo (mconcat $ coerce $ intersperse (showString "; ") $ map (displays . fromPositional) bs) . showString " end"
  displaysPrec n (Type ty)   = showParen (4 <= n) $ showString "type " . displaysPrec 5 (fromPositional ty)

type Env = I.Env Positional LargeType

data ElaborateError
  = NotStructure LargeType
  deriving (Eq, Show)

instance Display ElaborateError where
  display (NotStructure lty) = "not structure type: " ++ display (WithName lty)

data Path = Path Variable -- and list of small types
  deriving (Eq, Show)
  deriving Generic

instance Shift Path

instance DisplayName Path where
  displaysWithName _ (Path v) = displays v

instance ToType Path where
  toType (Path v) = I.TVar v

fromVariable :: Variable -> Path
fromVariable v = Path v

data LargeType
  = BaseType BaseType
  | Structure (Record LargeType)
  | AbstractType AbstractType
  | SemanticPath Path
  deriving (Eq, Show)
  deriving Generic

instance Shift LargeType

instance DisplayName LargeType where
  displaysWithName _ (BaseType b)       = displays b
  displaysWithName _ (Structure r)      = displaysWithName 0 r
  displaysWithName _ (AbstractType aty) = showString "[= " . displaysWithName 0 aty . showString "]"
  displaysWithName n (SemanticPath p)   = displaysWithName n p

getStructure :: Member (Error ElaborateError) r => LargeType -> Eff r (Record LargeType)
getStructure (Structure r) = return r
getStructure lty           = throwError $ NotStructure lty

newtype Quantified a = Quantified ([Positional IKind], a)
  deriving (Eq, Show)
  deriving Functor

class Quantification f where
  getKinds :: f a -> [IKind]
  getAnnotatedKinds :: f a -> [Positional IKind]
  qsLen :: f a -> Int
  enumVars :: f a -> [Variable]
  getBody :: f a -> a
  fromBody :: a -> f a
  quantify :: [Positional IKind] -> a -> f a
  qmap :: (a -> a) -> f a -> f a

instance Quantification Quantified where
  getKinds (Quantified (ks, _)) = map fromPositional ks
  getAnnotatedKinds (Quantified (ks, _)) = ks
  qsLen (Quantified (ks, _)) = length ks
  enumVars q = map variable [0..qsLen q-1]
  getBody (Quantified (_, x)) = x
  fromBody x = Quantified ([], x)
  quantify ks x = Quantified (ks, x)
  qmap f q = quantify (getAnnotatedKinds q) $ f $ getBody q

instance Shift a => Shift (Quantified a) where
  shiftAbove c d q = qmap (shiftAbove (c + qsLen q) d) q

newtype Existential a = Existential (Quantified a)
  deriving (Eq, Show)
  deriving Functor
  deriving Quantification
  deriving Shift

instance DisplayName a => DisplayName (Existential a) where
  displaysWithName _ (Existential (Quantified (ks, x))) =
    let ?nctx = newTypes $ length ks in
    let f = mconcat $ coerce $ intersperse (showString ", ") $ map (\(i, k) -> displayTypeVariable i . showString " : " . displays (fromPositional k)) $ zip [0..] ks in
    showString "∃" . appEndo f . showString ". " . displaysWithName 0 x

newtype Universal a = Universal (Quantified a)
  deriving (Eq, Show)
  deriving Functor
  deriving Quantification
  deriving Shift

type AbstractType = Existential LargeType

instance ToType AbstractType where
  toType (Existential (Quantified (ks, lty))) = I.some (map fromPositional ks) $ toType lty

data Purity
  = Pure
  | Impure
  deriving (Eq, Show)

instance Display Purity where
  display Pure   = "pure"
  display Impure = "impure"

-- Join.
instance Semigroup Purity where
  Pure <> Pure = Pure
  _ <> _       = Impure

class ToType a where
  toType :: a -> IType

instance ToType LargeType where
  toType (BaseType b)       = I.BaseType b
  toType (Structure r)      = toType r
  toType (AbstractType aty) = toType aty `I.TFun` I.TRecord (record [])
  toType (SemanticPath p)   = toType p

instance ToType a => ToType (Record a) where
  toType r = I.TRecord $ toType <$> r

toTerm :: AbstractType -> Term
toTerm aty = I.Abs (toType aty) $ I.TmRecord $ record []

translate :: Positional Expr -> Either I.Failure (Either ElaborateError (Term, AbstractType, Purity))
translate e = run $ runError $ runError $ evalFresh 0 $ let ?env = I.emptyEnv in elaborate e

runElaborate :: Eff '[Fresh, Error ElaborateError, Error I.Failure] a -> Either I.Failure (Either ElaborateError a)
runElaborate x = run $ runError $ runError $ evalFresh 0 x

class Elaboration a where
  type Output a
  type Effs a :: [* -> *]

  elaborate :: (Members (Effs a) r, ?env :: Env) => Positional a -> Eff r (Output a)

instance Elaboration Type where
  type Output Type = AbstractType
  type Effs Type = '[Error I.Failure, Error ElaborateError, Fresh]

  elaborate (Positional _ (Base b)) = return $ fromBody $ BaseType b
  elaborate (Positional p TypeType) = return $ quantify [positional p I.Base] $ AbstractType $ fromBody $ SemanticPath $ fromVariable $ variable 0

instance Elaboration Literal where
  type Output Literal = BaseType
  type Effs Literal = '[]

  elaborate = return . I.typeOfLiteral . fromPositional

instance Elaboration Expr where
  type Output Expr = (Term, AbstractType, Purity)
  type Effs Expr = '[Error I.Failure, Error ElaborateError, Fresh]

  elaborate (Positional pos (Lit l)) = do
    b <- elaborate $ Positional pos l
    return (I.Lit l, fromBody $ BaseType b, Pure) -- Literals are always pure.
  elaborate (Positional _ (Id id)) = do
    (lty, v) <- lookupValueByName $ coerce id
    return (I.Var v, fromBody lty, Pure)
  elaborate (Positional _ (Struct bs)) = do
    (_, aty, zs, p) <- foldlM elaborateBindings (?env, fromBody mempty, [], Pure) bs
    let lls = map (\(_, _, ls) -> ls) zs
    let t = I.TmRecord $ Record $ buildRecord lls
    let t1 = I.pack t (I.TVar <$> enumVars aty) (getKinds aty) $ toType $ getBody aty
    t <- foldlM joinBindings t1 zs
    return (t, Structure <$> aty, p)
  elaborate (Positional _ (Type ty)) = do
    aty <- elaborate ty
    return (toTerm aty, fromBody $ AbstractType aty, Pure)

buildRecord :: [[I.Label]] -> Map.Map I.Label Term
buildRecord lls = fst $ foldl f (mempty, 0) lls
  where
    f = foldr (\l (m', n') -> (Map.insertWith (\_ x -> x) l (var n') m', n' + 1))

joinBindings :: Member Fresh r => Term -> (Term, Int, [I.Label]) -> Eff r Term
joinBindings acc (t, n, ls) = do
  g <- I.generated <$> fresh
  return $ I.unpack (Just g) t n $ I.Let (map (I.Proj $ I.GVar g) ls) acc

type Acc = (Env, Existential (Record LargeType), [(Term, Int, [I.Label])], Purity)

elaborateBindings :: (Members (Effs Expr) r, ?env :: Env) => Acc -> Positional Binding -> Eff r Acc
elaborateBindings (env, whole_aty, zs, p0) b = do
  let ?env = env
  (t, aty, p) <- elaborate b
  let ?env = I.insertTypes $ reverse $ getAnnotatedKinds aty
  let r = getBody aty
  let ?env = foldl (\env (l, lty) -> let ?env = env in insertValue (toName l) lty) ?env $ I.toList r
  return (?env, merge whole_aty $ quantify (getAnnotatedKinds aty) r, (t, qsLen aty, I.labels r) : zs, p0 <> p)

merge :: Existential (Record LargeType) -> Existential (Record LargeType) -> Existential (Record LargeType)
merge aty1 aty2 =
  let ty1 = shift (qsLen aty2) $ getBody aty1 in
  quantify (getAnnotatedKinds aty2 ++ getAnnotatedKinds aty1) $ getBody aty2 <> ty1

instance Elaboration Binding where
  type Output Binding = (Term, Existential (Record LargeType), Purity)
  type Effs Binding = '[Error I.Failure, Error ElaborateError, Fresh]

  elaborate (Positional _ (Val id e)) = do
    (t, aty, p) <- elaborate e
    let l = I.toLabel $ coerce id
    return (I.unpack Nothing t (qsLen aty) $ I.pack (I.TmRecord $ record [(l, var 0)]) (I.TVar <$> enumVars aty) (getKinds aty) $ toType $ getBody aty, (\x -> record [(l, x)]) <$> aty, p)

  elaborate (Positional _ (Include e)) = do
    (t, aty, p) <- elaborate e
    r <- getStructure $ getBody aty
    return (t, quantify (getAnnotatedKinds aty) r, p)
