{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Modules.RRD2010
  ( Module(..)
  , Binding(..)
  , Expr(..)
  , Ident(..)
  , AbstractSig
  , SemanticSig(..)
  , Existential
  , existential
  , Elaboration(..)
  , TypeError
  , runEnv
  , var
  ) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Data.Bifunctor
import Data.Coerce
import Data.Foldable
import Data.Functor.Identity
import qualified Data.Map.Lazy as Map
import Data.Monoid
import qualified Data.Set as Set

import qualified Language.Modules.RRD2010.Internal as I

newtype Ident = Ident I.Name
  deriving (Eq, Ord, Show)

embedIntoLabel :: Ident -> I.Label
embedIntoLabel = I.Label . coerce

extractLabel :: Member (Error TypeError) r => I.Label -> Eff r I.Name
extractLabel (I.Label name) = return name
extractLabel l              = throwProblem $ NoCorrespondName l

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
  = ModuleIdent Ident
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

newtype Quantified a = Quantified ([I.Kind], a)
  deriving (Eq, Show, Functor)

newtype Existential a = Existential ([I.Kind], a)
  deriving (Eq, Show, Functor)
  deriving (I.Subst, I.Shift) via (Quantified a)

newtype Universal a = Universal ([I.Kind], a)
  deriving (Eq, Show, Functor)
  deriving (I.Subst, I.Shift) via (Quantified a)

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

isAtomic :: SemanticSig -> Bool
isAtomic (StructureSig _) = False
isAtomic (FunctorSig _)   = False
isAtomic _                = True

mkExistential :: [I.Kind] -> a -> Existential a
mkExistential = curry Existential

fromExistential :: Existential a -> a
fromExistential (Existential (_, x)) = x

existential :: a -> Existential a
existential x = Existential (mempty, x)

toUniversal :: Existential a -> Universal a
toUniversal = coerce

merge :: (Functor f, I.Shift a) => (a -> a -> f a) -> Existential a -> Existential a -> f (Existential a)
merge f (Existential (xs, x)) (Existential (ys, y)) = mkExistential (ys <> xs) <$> f (I.shift (length ys) x) y

instance I.Shift SemanticSig where
  shiftAbove c0 d = walk c0
    where
      walk c (AtomicTerm t)   = AtomicTerm $ I.shiftAbove c d t
      walk c (AtomicType t k) = AtomicType (I.shiftAbove c d t) k
      walk c (AtomicSig a)    = AtomicSig $ I.shiftAbove c d a
      walk c (StructureSig m) = StructureSig $ I.shiftAbove c d m
      walk c (FunctorSig u)   = FunctorSig $ I.shiftAbove c d u

instance I.Shift a => I.Shift (Quantified a) where
  shiftAbove c d (Quantified (ks, a)) = Quantified (ks, I.shiftAbove (c + length ks) d a)

instance (I.Shift a, I.Shift b) => I.Shift (Fun a b) where
  shiftAbove c d (x :-> y) = I.shiftAbove c d x :-> I.shiftAbove c d y

proj :: Member (Error TypeError) r => Foldable t => SemanticSig -> t Ident -> Eff r SemanticSig
proj = foldlM proj'

proj' :: Member (Error TypeError) r => SemanticSig -> Ident -> Eff r SemanticSig
proj' (StructureSig m) (embedIntoLabel -> l) =
  case Map.lookup l m of
    Nothing -> throwProblem $ NoSuchLabel m l
    Just ssig -> return ssig
proj' ssig _ = throwProblem $ NotStructureSig ssig

subst :: I.Subst a => Map.Map I.Variable I.Type -> a -> a
subst = I.substC 0

instance I.Subst a => I.Subst (Quantified a) where
  substC c s (Quantified (ks, x)) = Quantified (ks, I.substC (c + length ks) s x)

instance (I.Subst a, I.Subst b) => I.Subst (Fun a b) where
  substC c m (x :-> y) = I.substC c m x :-> I.substC c m y

instance I.Subst SemanticSig where
  substC c s (AtomicTerm ity)    = AtomicTerm $ I.substC c s ity
  substC c s (AtomicType ity ik) = AtomicType (I.substC c s ity) ik
  substC c s (AtomicSig asig)    = AtomicSig $ I.substC c s asig
  substC c s (StructureSig m)    = StructureSig $ I.substC c s <$> m
  substC c s (FunctorSig u)      = FunctorSig $ I.substC c s u

class Encode a where
  encode :: a -> I.Type

instance Encode a => Encode (Existential a) where
  encode (Existential (ks, x)) = I.some ks $ encode x

instance Encode a => Encode (Universal a) where
  encode (Universal (ks, x)) = I.forall ks $ encode x

instance (Encode a, Encode b) => Encode (Fun a b) where
  encode (x :-> y) = encode x `I.TFun` encode y

instance Encode SemanticSig where
  encode (AtomicTerm ity)    = I.TRecord $ coerce $ Map.singleton I.Val ity
  encode (AtomicType ity ik) = I.TRecord $ coerce $ Map.singleton I.Typ $ I.Forall (I.KFun ik I.Mono) $ I.TFun t t
    where v = I.Variable 0
          t = I.TApp (I.TVar v) ity
  encode (AtomicSig asig)    = I.TRecord $ coerce $ Map.singleton I.Sig $ encode asig `I.TFun` encode asig
  encode (StructureSig m)    = I.TRecord $ coerce $ encode <$> m
  encode (FunctorSig u)      = encode u

data SemanticTerm
  = STerm I.Term
  | SType I.Type I.Kind
  | SAbstractSig AbstractSig
  deriving (Eq, Show)

var :: Int -> I.Term
var = I.Var . coerce

encodeAsTerm :: SemanticTerm -> I.Term
encodeAsTerm (STerm t)           = I.TmRecord $ coerce $ Map.singleton I.Val $ t
encodeAsTerm (SType ity ik)      = I.TmRecord $ coerce $ Map.singleton I.Typ $ I.Poly (I.KFun ik I.Mono) $ I.Abs t $ var 0
  where t = I.TApp (I.TVar $ I.Variable 0) ity
encodeAsTerm (SAbstractSig asig) = I.TmRecord $ coerce $ Map.singleton I.Sig $ I.Abs (encode asig) $ var 0

data TypeError = TypeError [Reason] Problem
  deriving (Eq, Show)

data Reason
  = ExpectMono I.Type
  deriving (Eq, Show)

data Problem
  = NotMono I.Kind
  | NoCorrespondName I.Label
  | IncludeNonStructureSig SemanticSig
  | DuplicateDecls (Map.Map I.Label SemanticSig) (Map.Map I.Label SemanticSig)
  | NotStructureSig SemanticSig
  | NoSuchLabel (Map.Map I.Label SemanticSig) I.Label
  | KindMismatch I.Kind I.Kind
  | NotAbstractType I.Variable
  | NotTypeVariable SemanticSig
  | NotEqual I.Type I.Type
  | NoInstantiation
  | NotSubmap (Map.Map I.Label SemanticSig) (Map.Map I.Label SemanticSig)
  | StructuralMismatch SemanticSig SemanticSig
  | NotAtomic SemanticSig
  | NoSuchIdent Ident (I.Env SemanticSig)
  | NotFunctor SemanticSig
  | NotType Path
  | NotTerm Path
  | NotSig Path
  | Internal I.InternalTypeError
  deriving (Eq, Show)

fromProblem :: Problem -> TypeError
fromProblem = TypeError []

throwProblem :: Member (Error TypeError) r => Problem -> Eff r a
throwProblem = throwError . fromProblem

addReason :: Reason -> TypeError -> TypeError
addReason r (TypeError rs p) = TypeError (r : rs) p

annotate :: Member (Error TypeError) r => Eff r a -> Reason -> Eff r a
annotate x r = x `catchError` (throwError . addReason r)

type Env = '[State (I.Env SemanticSig), Error TypeError]

runEnv :: I.Env b -> Eff '[State (I.Env b), Error TypeError] a -> Either TypeError a
runEnv e x = run $ runError $ evalState e x

class Elaboration a where
  type Output a
  elaborate :: Members Env r => a -> Eff r (Output a)

instance Elaboration Kind where
  type Output Kind = I.Kind

  elaborate Mono = return I.Mono

instance Elaboration Type where
  type Output Type = (I.Type, I.Kind)

  elaborate Int          = return (I.Int, I.Mono)
  elaborate (PathType p) = do
    ssig <- snd <$> elaborate p
    case ssig of
      AtomicType t k -> return (t, k)
      _              -> throwProblem $ NotType p

getEnv :: Member (State (I.Env SemanticSig)) r => Eff r (I.Env SemanticSig)
getEnv = get

transaction :: Member (State (I.Env SemanticSig)) r => Eff r a -> Eff r a
transaction e = do
  env <- getEnv
  x <- e
  put env
  return x

insertKind :: I.Kind -> I.Env SemanticSig -> I.Env SemanticSig
insertKind = I.insertKind

updateEnvWithVars :: Members Env r => [I.Kind] -> Eff r ()
updateEnvWithVars = modify . foldr (\k f -> insertKind k . f) id

insertSemSig :: I.Name -> SemanticSig -> I.Env SemanticSig -> I.Env SemanticSig
insertSemSig = I.insertType

updateEnv :: Members Env r => Existential (Map.Map I.Label SemanticSig) -> Eff r ()
updateEnv (Existential (ks, m)) = do
  updateEnvWithVars ks
  _ <- Map.traverseWithKey (\l s -> extractLabel l >>= \name -> modify $ insertSemSig name s) m
  return ()

instance Elaboration Sig where
  type Output Sig = AbstractSig

  elaborate (SigPath p) = do
    ssig <- snd <$> elaborate p
    case ssig of
      AtomicSig asig -> return asig
      _              -> throwProblem $ NotSig p

  elaborate (Decls ds) = transaction $ mapM f ds >>= fmap (fmap StructureSig) . foldrM (merge g) (Existential mempty)
    where
      f :: Members Env r => Decl -> Eff r (Existential (Map.Map I.Label SemanticSig))
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
    Existential (ks, ssig) <- elaborate sig
    (ity, ik) <- elaborate ty
    ssig' <- proj ssig p
    case ssig' of
      AtomicType (I.TVar v) ik'
        | coerce v >= length ks -> throwProblem $ NotAbstractType v
        | ik' == ik             -> return $ Existential (removeNth (coerce v) ks, subst (Map.singleton v ity) ssig)
        | otherwise             -> throwProblem $ KindMismatch ik ik'
      _                         -> throwProblem $ NotTypeVariable ssig'

removeNth :: Int -> [a] -> [a]
removeNth n xs =
  let (ys, zs) = splitAt n xs in
    ys ++ tail zs

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
    [ Existential ([ik], Map.singleton (embedIntoLabel i) $ AtomicType (I.TVar $ I.Variable 0) ik)
    | ik <- elaborate k
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

variables :: [a] -> [I.Variable]
variables xs = coerce [0 .. length xs - 1]

instance Subtype I.Type where
  t1 <: t2
    | t1 .= t2  = return $ I.Abs t1 $ var 0
    | otherwise = throwProblem $ NotEqual t1 t2

match :: Members Env r => SemanticSig -> AbstractSig -> Eff r (I.Term, Map.Map I.Variable I.Type)
match ssig (Existential (ks, s)) = do
  m <- sequence $ fmap f $ coerce $ Map.fromSet (lookupInst ssig s) $ Set.fromList $ variables ks
  t <- ssig <: subst m s
  return (t, m)
  where
    f :: Member (Error TypeError) r => Maybe I.Type -> Eff r I.Type
    f Nothing   = throwProblem NoInstantiation
    f (Just ty) = return ty

pack :: AbstractSig -> [I.Type] -> I.Term -> I.Term
pack (Existential (ks, ssig)) = I.pack ks $ encode ssig

unpack :: AbstractSig -> I.Term -> I.Term -> I.Term
unpack (Existential (ks, ssig)) = I.unpack (encode ssig) $ length ks

instance Subtype AbstractSig where
  a @ (Existential (ks1, ssig1)) <: b = transaction $ do
    updateEnvWithVars ks1
    (c, ts) <- ssig1 `match` b
    return $ I.Abs (encode a) $ unpack a (var 0) $ pack b (Map.elems ts) $ I.App c $ var 0

instance Subtype SemanticSig where
  AtomicTerm t <: AtomicTerm u =
    [ I.Abs (encode $ AtomicTerm t) $ I.App c $ I.Proj (var 0) I.Val
    | c <- t <: u
    ]

  s @ (AtomicType t k) <: AtomicType u l
    | k /= l    = throwProblem $ KindMismatch k l
    | t .= u    = return $ I.Abs (encode s) $ var 0
    | otherwise = throwProblem $ NotEqual t u

  AtomicSig a <: AtomicSig b = do
    _ <- a <: b
    _ <- b <: a
    return $ I.Abs (encode a) $ encodeAsTerm $ SAbstractSig b

  s @ (StructureSig m) <: StructureSig n
    | Map.keysSet n `Set.isSubsetOf` Map.keysSet m =
      [ I.Abs (encode s) $ I.TmRecord $ coerce $ Map.mapWithKey (\l c -> I.App c $ I.Var (I.Variable 0) `I.Proj` l) o
      | o <- sequence $ Map.intersectionWith (<:) m n
      ]
    | otherwise                                    = throwProblem $ NotSubmap n m

  ssig @ (FunctorSig (Universal (ks1, s :-> a))) <: FunctorSig (Universal (ks2, t :-> b)) = transaction $ do
    updateEnvWithVars ks2
    (c, ts) <- t `match` Existential (ks1, s)
    d <- subst ts a <: b
    return $ I.Abs (encode ssig) $ I.poly ks2 $ I.Abs (encode t) $ I.App d $ I.inst (var 1) (Map.elems ts) `I.App` I.App c (var 0)

  x <: y = throwProblem $ StructuralMismatch x y

instance Elaboration Expr where
  type Output Expr = (I.Term, I.Type)

  elaborate (IntLit n)   = return (I.IntLit n, I.Int)
  elaborate (PathExpr p) = elaborate p >>= sequence . bimap (`I.Proj` I.Val) f
    where
      f :: Member (Error TypeError) r => SemanticSig -> Eff r I.Type
      f (AtomicTerm t) = return t
      f _              = throwProblem $ NotTerm p

instance Elaboration Module where
  type Output Module = (I.Term, AbstractSig)

  elaborate (ModuleIdent i) = do
    env <- getEnv
    (ssig, n) <- lookupTypeByIdent i env
    return (var n, existential ssig)

  elaborate (Bindings bs) = transaction $ do
    ps <- traverse f bs
    let asig @ (Existential (ks, _)) = fmap StructureSig $ runIdentity $ foldrM (merge g) (existential mempty) $ snd <$> ps
    let r = fst $ foldr f1 (mempty, 0) $ fromExistential . snd <$> ps
    let t = foldr ($) (pack asig (I.TVar <$> variables ks) $ I.TmRecord r) $ map h ps
    return (t, asig)
      where
        f :: Members Env r => Binding -> Eff r (I.Term, Existential (Map.Map I.Label SemanticSig))
        f b = do
          (t, m) <- elaborate b
          getEnv >>= put . I.insertNothing
          updateEnv m
          return (t, m)

        g :: Applicative f => Map.Map I.Label SemanticSig -> Map.Map I.Label SemanticSig -> f (Map.Map I.Label SemanticSig)
        g m1 m2 = pure $ Map.union m2 m1

        h :: (I.Term, Existential (Map.Map I.Label SemanticSig)) -> I.Term -> I.Term
        h (t, e) t0 =
          unpack (StructureSig <$> e) t $
          I.let_ (bimap (I.Proj $ var 0) encode <$> Map.toList (fromExistential e)) t0

        f1 :: Map.Map I.Label SemanticSig -> (I.Record I.Term, Int) -> (I.Record I.Term, Int)
        f1 m (r0, n) =
          let n' = n + Map.size m in
          let w = (`foldMap` Map.keysSet m) $ \l ->
                Endo $ case coerce r0 Map.!? l :: Maybe I.Term of
                  Just _  -> id
                  Nothing -> coerce $ Map.insert l (var n' `I.Proj` l) in
          (appEndo w r0, n' + 1)

  elaborate (Projection m i) = do
    (c, asig @ (Existential (ks, ssig))) <- elaborate m
    ssig' <- proj' ssig i
    let ts = I.TVar <$> variables ks
    return (unpack asig c $ I.pack ks (encode ssig') ts $ I.Proj (var 0) $ embedIntoLabel i, Existential (ks, ssig'))

  elaborate (Fun i sig m) = transaction $ do
    Existential (ks, ssig) <- elaborate sig
    updateEnvWithVars ks
    modify $ insertSemSig (coerce i) ssig
    (c, asig) <- elaborate m
    return (I.poly ks $ I.Abs (encode ssig) c, existential $ FunctorSig $ Universal (ks, ssig :-> asig))

  elaborate (ModuleApp i j) = do
    env <- getEnv
    (ssig0, m) <- lookupTypeByIdent i env
    Universal (ks, ssig' :-> asig) <- fromFunctor ssig0
    (ssig, n) <- lookupTypeByIdent j env
    (c, ts) <- ssig `match` Existential (ks, ssig')
    return (I.App (I.inst (var m) $ Map.elems ts) (I.App c $ var n), subst ts asig)

  elaborate (i :> sig) = do
    env <- getEnv
    (ssig, n) <- lookupTypeByIdent i env
    asig <- elaborate sig
    (c, ts) <- ssig `match` asig
    return (pack asig (Map.elems ts) $ I.App c $ var n, asig)

lookupTypeByIdent :: Member (Error TypeError) r => Ident -> I.Env SemanticSig -> Eff r (SemanticSig, Int)
lookupTypeByIdent i env = maybe (throwProblem $ NoSuchIdent i env) return $ I.lookupTypeByName (coerce i) env

fromFunctor :: Member (Error TypeError) r => SemanticSig -> Eff r (Universal (Fun SemanticSig AbstractSig))
fromFunctor (FunctorSig u) = return u
fromFunctor ssig           = throwProblem $ NotFunctor ssig

instance Elaboration Binding where
  type Output Binding = (I.Term, Existential (Map.Map I.Label SemanticSig))

  elaborate (Val i e) =
    [ (atomicTerm i $ STerm c, atomic i $ AtomicTerm ty)
    | (c, ty) <- elaborate e
    ]

  elaborate (Type i ty) =
    [ (atomicTerm i $ SType ity ik, atomic i $ AtomicType ity ik)
    | (ity, ik) <- elaborate ty
    ]

  elaborate (Module i m) = do
    (c, asig @ (Existential (ks, ssig))) <- elaborate m

    when (isAtomic ssig) $
      throwProblem $ NotAtomic ssig

    let ty = Map.singleton (embedIntoLabel i) <$> asig
    let ts = I.TVar <$> variables ks
    return (unpack asig c $ I.pack ks (encode $ StructureSig $ fromExistential ty) ts $ I.TmRecord $ coerce $ Map.singleton (embedIntoLabel i) $ var 0, ty)

  elaborate (Signature i sig) =
    [ (atomicTerm i $ SAbstractSig asig, atomic i $ AtomicSig asig)
    | asig <- elaborate sig
    ]

  elaborate (Include m) = do
    (c, asig) <- elaborate m
    case fromExistential asig of
      StructureSig s -> return (c, const s <$> asig)
      ssig           -> throwProblem $ NotStructureSig ssig

atomicTerm :: Ident -> SemanticTerm -> I.Term
atomicTerm i = I.TmRecord . coerce . Map.singleton (embedIntoLabel i) . encodeAsTerm

kindOf :: Members Env r => I.Type -> Eff r I.Kind
kindOf t = do
  env <- getEnv
  case run $ runError $ runReader env $ I.kindOf t of
    Right k -> return k
    Left e  -> throwProblem $ Internal e

instance Elaboration Path where
  type Output Path = (I.Term, SemanticSig)

  elaborate (Path m) = do
    (t, e) <- elaborate m
    kindOf (encode $ fromExistential e) >>= expectMono
    return (unpack e t $ var 0, fromExistential e)
