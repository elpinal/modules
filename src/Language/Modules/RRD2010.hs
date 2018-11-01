{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Modules.RRD2010
  (
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.State
import Data.Coerce
import Data.Foldable
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set

import qualified Language.Modules.RRD2010.Internal as I

newtype Ident = Ident Int
  deriving (Eq, Ord, Show)

class Embed a where
  type Target a
  embed :: a -> Target a
  extract :: Target a -> Maybe a

instance Embed Ident where
  type Target Ident = I.Variable
  embed = coerce
  extract = Just . coerce

instance Embed I.Variable where
  type Target I.Variable = I.Label
  embed = I.Label . coerce

  extract (I.Label l) = Just $ coerce l
  extract _ = Nothing

embedIntoLabel :: Ident -> I.Label
embedIntoLabel = embed . embed

extractLabel :: Member (Error TypeError) r => I.Label -> Eff r I.Variable
extractLabel (extract -> Just x) = return x
extractLabel l                   = throwError $ fromProblem $ NoCorrespondVariable l

data Kind = Mono
  deriving (Eq, Show)

data Type
  = Int
  | PathType Path
  deriving (Eq, Show)

data Expr
  = IntLit Int
  | PathExpr Path
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

-- TODO: perhaps instead of Ident, I.Variable should be used here.
data Existential a = Existential (Map.Map Ident I.Kind) a
  deriving (Eq, Show, Functor)

data Universal a = Universal (Map.Map Ident I.Kind) a
  deriving (Eq, Show, Functor)

type AbstractSig = Existential SemanticSig

data Fun a b = a :-> b
  deriving (Eq, Show)

data SemanticSig
  = AtomicTerm I.Type
  | AtomicType I.Type I.Kind
  | AtomicSig AbstractSig
  | StructureSig (Map.Map I.Label SemanticSig)
  | FunctorSig (Universal (Fun SemanticSig AbstractSig))
  deriving (Eq, Show)

existential :: a -> Existential a
existential = Existential mempty

toUniversal :: Existential a -> Universal a
toUniversal (Existential m x) = Universal m x

merge :: Functor f => (a -> a -> f a) -> Existential a -> Existential a -> f (Existential a)
merge f (Existential m1 x) (Existential m2 y) = Existential (Map.union m1 m2) <$> f x y

proj :: Member (Error TypeError) r => Foldable t => SemanticSig -> t Ident -> Eff r SemanticSig
proj = foldlM proj'

proj' :: Member (Error TypeError) r => SemanticSig -> Ident -> Eff r SemanticSig
proj' (StructureSig m) (embedIntoLabel -> l) =
  case Map.lookup l m of
    Nothing -> throwError $ fromProblem $ NoSuchLabel m l
    Just ssig -> return ssig
proj' ssig _ = throwError $ fromProblem $ NotStructureSig ssig

class Encode a where
  encode :: a -> I.Type

instance Encode a => Encode (Existential a) where
  encode (Existential is x) = I.some (Map.mapKeys coerce is) $ encode x

instance Encode a => Encode (Universal a) where
  encode (Universal is x) = I.forall (Map.mapKeys coerce is) $ encode x

instance (Encode a, Encode b) => Encode (Fun a b) where
  encode (x :-> y) = encode x `I.TFun` encode y

instance Encode SemanticSig where
  encode (AtomicTerm ity)    = I.TRecord $ coerce $ Map.singleton I.Val ity
  encode (AtomicType ity ik) = I.TRecord $ coerce $ Map.singleton I.Typ $ I.Forall v (I.KFun ik I.Mono) $ I.TFun t t
    where v = I.Variable 0
          t = I.TApp (I.TVar v) ity
  encode (AtomicSig asig)    = I.TRecord $ coerce $ Map.singleton I.Sig $ encode asig `I.TFun` encode asig
  encode (StructureSig m)    = I.TRecord $ coerce $ encode <$> m
  encode (FunctorSig u)      = encode u

data TypeError = TypeError [Reason] Problem
  deriving (Eq, Show)

data Reason
  = ExpectMono I.Type
  deriving (Eq, Show)

data Problem
  = NotMono I.Kind
  | NoCorrespondVariable I.Label
  | IncludeNonStructureSig SemanticSig
  | DuplicateDecls (Map.Map I.Label SemanticSig) (Map.Map I.Label SemanticSig)
  | NotStructureSig SemanticSig
  | NoSuchLabel (Map.Map I.Label SemanticSig) I.Label
  deriving (Eq, Show)

fromProblem :: Problem -> TypeError
fromProblem = TypeError []

addReason :: Reason -> TypeError -> TypeError
addReason r (TypeError rs p) = TypeError (r : rs) p

annotate :: Member (Error TypeError) r => Eff r a -> Reason -> Eff r a
annotate x r = x `catchError` (throwError . addReason r)

type Env = '[State I.Env, Error TypeError]

class Elaboration a where
  type Output a
  elaborate :: Members (Fresh ': Env) r => a -> Eff r (Output a)

instance Elaboration Kind where
  type Output Kind = I.Kind

  elaborate Mono = return I.Mono

instance Elaboration Type where
  type Output Type = (I.Type, I.Kind)

  elaborate Int = return (I.Int, I.Mono)

transaction :: Member (State I.Env) r => Eff r a -> Eff r a
transaction e = do
  env <- get
  x <- e
  put (env :: I.Env)
  return x

updateEnv :: Members Env r => Existential (Map.Map I.Label SemanticSig) -> Eff r ()
updateEnv (Existential is m) = do
  _ <- Map.traverseWithKey (\i -> modify . I.insertKind (coerce i)) is
  _ <- Map.traverseWithKey (\l s -> extractLabel l >>= \v -> modify $ I.insertType v $ encode s) m
  return ()

instance Elaboration Sig where
  type Output Sig = AbstractSig

  elaborate (Decls ds) = transaction $ mapM f ds >>= fmap (fmap StructureSig) . foldrM (merge g) (Existential mempty mempty)
    where
      f :: Members (Fresh ': Env) r => Decl -> Eff r (Existential (Map.Map I.Label SemanticSig))
      f d = do
        e <- elaborate d
        updateEnv e
        return e

      g m1 m2 = do
        if Map.keysSet m1 `Set.disjoint` Map.keysSet m2
          then return $ m1 <> m2
          else throwError $ fromProblem $ DuplicateDecls m1 m2

  elaborate (FunSig i sig1 sig2) = transaction $ do
    asig1 <- elaborate sig1
    updateEnv $ Map.singleton (embedIntoLabel i) <$> asig1
    asig2 <- elaborate sig2
    return $ existential $ FunctorSig $ (:-> asig2) <$> toUniversal asig1

atomic :: Ident -> a -> Existential (Map.Map I.Label a)
atomic i = existential . Map.singleton (embedIntoLabel i)

instance Elaboration Decl where
  type Output Decl = Existential (Map.Map I.Label SemanticSig)

  elaborate (ValDecl i ty) =
    [ atomic i $ AtomicTerm ity
    | ity <- elaborate ty >>= extractMonoType
    ]

  elaborate (ManTypeDecl i ty) =
    [ atomic i $ AtomicType ity ik
    | (ity, ik) <- elaborate ty
    ]

  elaborate (AbsTypeDecl i k) =
    [ Existential (Map.singleton (coerce n) ik) $ Map.singleton (embedIntoLabel i) $ AtomicType (I.TVar $ coerce n) ik
    | n <- fresh
    , ik <- elaborate k
    ]

  elaborate (ModuleDecl i s) =
    [ Map.singleton (embedIntoLabel i) <$> asig
    | asig <- elaborate s
    ]

  elaborate (SignatureDecl i s) =
    [ atomic i $ AtomicSig asig
    | asig <- elaborate s
    ]

  elaborate (IncludeDecl s) = do
    Existential m ssig <- elaborate s
    case ssig of
      StructureSig x -> return $ Existential m x
      _              -> throwError $ fromProblem $ IncludeNonStructureSig ssig

extractMonoType :: Member (Error TypeError) r => (I.Type, I.Kind) -> Eff r I.Type
extractMonoType (ity, ik) = do
  expectMono ik `annotate` ExpectMono ity
  return ity

expectMono :: Member (Error TypeError) r => I.Kind -> Eff r ()
expectMono I.Mono = return ()
expectMono ik     = throwError $ fromProblem $ NotMono ik
