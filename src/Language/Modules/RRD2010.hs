{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Modules.RRD2010
  (
  ) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Fresh
import Control.Monad.Freer.State
import Data.Coerce
import Data.Foldable
import qualified Data.Map.Lazy as Map
import Data.Monoid
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
extractLabel l                   = throwProblem $ NoCorrespondVariable l

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

newtype Quantified a = Quantified (Map.Map I.Variable I.Kind, a)
  deriving (Eq, Show, Functor)

newtype Existential a = Existential (Map.Map I.Variable I.Kind, a)
  deriving (Eq, Show, Functor)
  deriving Subst via (Quantified a)

newtype Universal a = Universal (Map.Map I.Variable I.Kind, a)
  deriving (Eq, Show, Functor)
  deriving Subst via (Quantified a)

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

mkExistential :: Map.Map I.Variable I.Kind -> a -> Existential a
mkExistential = curry Existential

existential :: a -> Existential a
existential x = Existential (mempty, x)

toUniversal :: Existential a -> Universal a
toUniversal = coerce

merge :: Functor f => (a -> a -> f a) -> Existential a -> Existential a -> f (Existential a)
merge f (Existential (m1, x)) (Existential (m2, y)) = mkExistential (Map.union m1 m2) <$> f x y

proj :: Member (Error TypeError) r => Foldable t => SemanticSig -> t Ident -> Eff r SemanticSig
proj = foldlM proj'

proj' :: Member (Error TypeError) r => SemanticSig -> Ident -> Eff r SemanticSig
proj' (StructureSig m) (embedIntoLabel -> l) =
  case Map.lookup l m of
    Nothing -> throwProblem $ NoSuchLabel m l
    Just ssig -> return ssig
proj' ssig _ = throwProblem $ NotStructureSig ssig

class Subst a where
  subst :: Map.Map I.Variable I.Type -> a -> a

instance Subst a => Subst (Quantified a) where
  subst s (Quantified (m, x)) = Quantified (m, subst (s Map.\\ m) x) -- TODO: It might cause variable capturing.

instance (Subst a, Subst b) => Subst (Fun a b) where
  subst m (x :-> y) = subst m x :-> subst m y

sub :: Map.Map I.Variable I.Type -> I.Variable -> I.Type -> I.Type
sub m v ty = subst (Map.delete v m) ty

-- TODO: It might cause variable capturing.
instance Subst I.Type where
  subst m ty @ (I.TVar v)   = Map.findWithDefault ty v m
  subst m (I.TFun ty1 ty2)  = I.TFun (subst m ty1) (subst m ty2)
  subst m (I.TRecord rt)    = I.TRecord $ subst m<$> rt
  subst m (I.Forall v k ty) = I.Forall v k $ sub m v ty
  subst m (I.Some v k ty)   = I.Some v k $ sub m v ty
  subst m (I.TAbs v k ty)   = I.TAbs v k $ sub m v ty
  subst m (I.TApp ty1 ty2)  = I.TApp (subst m ty1) (subst m ty2)
  subst _ I.Int             = I.Int

instance Subst SemanticSig where
  subst s (AtomicTerm ity)    = AtomicTerm $ subst s ity
  subst s (AtomicType ity ik) = AtomicType (subst s ity) ik
  subst s (AtomicSig asig)    = AtomicSig $ subst s asig
  subst s (StructureSig m)    = StructureSig $ subst s <$> m
  subst s (FunctorSig u)      = FunctorSig $ subst s u

-- FIXME: Don't use.
tempVar :: I.Variable
tempVar = I.Variable 0

class Encode a where
  encode :: a -> I.Type

instance Encode a => Encode (Existential a) where
  encode (Existential (m, x)) = I.some (Map.mapKeys coerce m) $ encode x

instance Encode a => Encode (Universal a) where
  encode (Universal (m, x)) = I.forall (Map.mapKeys coerce m) $ encode x

instance (Encode a, Encode b) => Encode (Fun a b) where
  encode (x :-> y) = encode x `I.TFun` encode y

instance Encode SemanticSig where
  encode (AtomicTerm ity)    = I.TRecord $ coerce $ Map.singleton I.Val ity
  encode (AtomicType ity ik) = I.TRecord $ coerce $ Map.singleton I.Typ $ I.Forall v (I.KFun ik I.Mono) $ I.TFun t t
    where v = tempVar
          t = I.TApp (I.TVar v) ity
  encode (AtomicSig asig)    = I.TRecord $ coerce $ Map.singleton I.Sig $ encode asig `I.TFun` encode asig
  encode (StructureSig m)    = I.TRecord $ coerce $ encode <$> m
  encode (FunctorSig u)      = encode u

data SemanticTerm
  = STerm I.Term
  | SType I.Type I.Kind
  | SAbstractSig AbstractSig
  deriving (Eq, Show)

encodeAsTerm :: SemanticTerm -> I.Term
encodeAsTerm (STerm t)           = I.TmRecord $ coerce $ Map.singleton I.Val $ t
encodeAsTerm (SType ity ik)      = I.TmRecord $ coerce $ Map.singleton I.Typ $ I.Poly v (I.KFun ik I.Mono) $ I.Abs v1 t $ I.Var v1
  where v = tempVar
        v1 = I.Variable 1 -- FIXME
        t = I.TApp (I.TVar v) ity
encodeAsTerm (SAbstractSig asig) = I.TmRecord $ coerce $ Map.singleton I.Sig $ I.Abs v (encode asig) $ I.Var v
  where v = tempVar

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
  | KindMismatch I.Kind I.Kind
  | NotAbstractType I.Variable
  | NotTypeVariable SemanticSig
  | NotEqual I.Type I.Type
  | NoInstantiation I.Variable
  | NotSubmap (Map.Map I.Label SemanticSig) (Map.Map I.Label SemanticSig)
  deriving (Eq, Show)

fromProblem :: Problem -> TypeError
fromProblem = TypeError []

throwProblem :: Member (Error TypeError) r => Problem -> Eff r a
throwProblem = throwError . fromProblem

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

updateEnvWithVars :: Members Env r => Map.Map I.Variable I.Kind -> Eff r ()
updateEnvWithVars = void . Map.traverseWithKey (\v -> modify . I.insertKind v)

updateEnv :: Members Env r => Existential (Map.Map I.Label SemanticSig) -> Eff r ()
updateEnv (Existential (vs, m)) = do
  updateEnvWithVars vs
  _ <- Map.traverseWithKey (\l s -> extractLabel l >>= \v -> modify $ I.insertType v $ encode s) m
  return ()

instance Elaboration Sig where
  type Output Sig = AbstractSig

  elaborate (Decls ds) = transaction $ mapM f ds >>= fmap (fmap StructureSig) . foldrM (merge g) (Existential mempty)
    where
      f :: Members (Fresh ': Env) r => Decl -> Eff r (Existential (Map.Map I.Label SemanticSig))
      f d = do
        e <- elaborate d
        updateEnv e
        return e

      g m1 m2 = do
        if Map.keysSet m1 `Set.disjoint` Map.keysSet m2
          then return $ m1 <> m2
          else throwProblem $ DuplicateDecls m1 m2

  elaborate (FunSig i sig1 sig2) = transaction $ do
    asig1 <- elaborate sig1
    updateEnv $ Map.singleton (embedIntoLabel i) <$> asig1
    asig2 <- elaborate sig2
    return $ existential $ FunctorSig $ (:-> asig2) <$> toUniversal asig1

  elaborate (Where sig p ty) = do
    Existential (m, ssig) <- elaborate sig
    (ity, ik) <- elaborate ty
    ssig' <- proj ssig p
    case ssig' of
      AtomicType (I.TVar v) ik'
        | ik' == ik, v `Map.member` m -> return $ Existential (v `Map.delete` m, subst (Map.singleton v ity) ssig)
        | ik' == ik                   -> throwProblem $ KindMismatch ik ik'
        | otherwise                   -> throwProblem $ NotAbstractType v
      _                               -> throwProblem $ NotTypeVariable ssig'

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
    [ Existential (Map.singleton (coerce n) ik, Map.singleton (embedIntoLabel i) $ AtomicType (I.TVar $ coerce n) ik)
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
    Existential (m, ssig) <- elaborate s
    case ssig of
      StructureSig x -> return $ Existential (m, x)
      _              -> throwProblem $ IncludeNonStructureSig ssig

extractMonoType :: Member (Error TypeError) r => (I.Type, I.Kind) -> Eff r I.Type
extractMonoType (ity, ik) = do
  expectMono ik `annotate` ExpectMono ity
  return ity

expectMono :: Member (Error TypeError) r => I.Kind -> Eff r ()
expectMono I.Mono = return ()
expectMono ik     = throwProblem $ NotMono ik

lookupInst :: SemanticSig -> SemanticSig -> I.Variable -> First I.Type
lookupInst (AtomicType ty1 k1) (AtomicType (I.TVar v) k2) v0
  | k1 == k2, v == v0                            = return ty1
  | otherwise                                    = First Nothing
lookupInst (StructureSig m1) (StructureSig m2) v = foldMap (\(x, y) -> lookupInst x y v) $ Map.intersectionWith (,) m1 m2
lookupInst _ _ _                                 = First Nothing

class Subtype a where
  (<:) :: Members Env r => a -> a -> Eff r I.Term

(.=) :: I.Type -> I.Type -> Bool
(.=) = (==)

instance Subtype I.Type where
  t1 <: t2
    | t1 .= t2  = return $ I.Abs tempVar t1 $ I.Var tempVar
    | otherwise = throwProblem $ NotEqual t1 t2

match :: Members Env r => SemanticSig -> AbstractSig -> Eff r (I.Term, Map.Map I.Variable I.Type)
match ssig (Existential (m, s)) = do
  m1 <- Map.traverseWithKey f $ Map.fromSet (coerce . lookupInst ssig s) $ Map.keysSet m
  t <- ssig <: subst m1 s
  return (t, m1)
  where
    f :: Member (Error TypeError) r => I.Variable -> Maybe I.Type -> Eff r I.Type
    f v Nothing   = throwProblem $ NoInstantiation v
    f _ (Just ty) = return ty

instance Subtype a => Subtype (Existential a) where
  (<:) = undefined

instance Subtype SemanticSig where
  AtomicTerm t <: AtomicTerm u =
    let v = tempVar in
      [ I.Abs v t $ I.App c $ I.Proj (I.Var v) I.Val
      | c <- t <: u
      ]

  s @ (AtomicType t k) <: AtomicType u l
    | k /= l    = throwProblem $ KindMismatch k l
    | t .= u    = return $ I.Abs tempVar (encode s) $ I.Var tempVar
    | otherwise = throwProblem $ NotEqual t u

  AtomicSig a <: AtomicSig b = do
    _ <- a <: b
    _ <- b <: a
    return $ I.Abs tempVar (encode a) $ encodeAsTerm $ SAbstractSig b

  s @ (StructureSig m) <: StructureSig n
    | Map.keysSet n `Set.isSubsetOf` Map.keysSet m =
      let v = tempVar in
        [ I.Abs v (encode s) $ I.TmRecord $ coerce $ Map.mapWithKey (\l c -> I.App c $ I.Var v `I.Proj` l) o
        | o <- sequence $ Map.intersectionWith (<:) m n
        ]
    | otherwise                                    = throwProblem $ NotSubmap n m

  ssig @ (FunctorSig (Universal (m, s :-> a))) <: FunctorSig (Universal (n, t :-> b)) = transaction $ do
    updateEnvWithVars n
    (c, o) <- t `match` Existential (m, s)
    d <- subst o a <: b
    return $ I.Abs v (encode ssig) $ I.poly n $ I.Abs v1 (encode t) $ I.App d $ I.inst (I.Var v) o `I.App` I.App c (I.Var v1)
      where v = tempVar
            v1 = I.Variable 1 -- FIXME
