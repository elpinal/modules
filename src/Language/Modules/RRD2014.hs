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

{-|
Module  : Language.Modules.RRD2014
License : MIT

Andreas Rossberg, Claudio V. Russo and Derek Dreyer.
F-ing modules.
Journal of Functional Programming, 24(5), 2014.
-}

module Language.Modules.RRD2014
  ( Module(..)
  , Binding(..)
  , Sig(..)
  , Proj(..)
  , Decl(..)
  , Path(..)
  , Expr(..)
  , Type(..)
  , Kind(..)
  , Ident(..)
  , AbstractSig
  , SemanticSig(..)
  , Existential
  , existential
  , Encode(..)
  , Elaboration(..)
  , TypeError
  , fromProblem
  , Problem(..)
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
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Monoid
import qualified Data.Set as Set

import qualified Language.Modules.RRD2014.Internal as I

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
  | TFun Type Type
  | PathType Path
  | Package Sig
  deriving (Eq, Show)

data Expr
  = IntLit Int
  | Var Ident
  | Abs Ident Type Expr
  | App Expr Expr
  | PathExpr Path
  | Pack Module Sig
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
  | Unpack Expr Sig
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
  substCS su c s (Quantified (ks, x)) = Quantified (ks, I.substCS su (c + length ks) s x)

instance (I.Subst a, I.Subst b) => I.Subst (Fun a b) where
  substCS su c m (x :-> y) = I.substCS su c m x :-> I.substCS su c m y

instance I.Subst SemanticSig where
  substCS su c s (AtomicTerm ity)    = AtomicTerm $ I.substCS su c s ity
  substCS su c s (AtomicType ity ik) = AtomicType (I.substCS su c s ity) ik
  substCS su c s (AtomicSig asig)    = AtomicSig $ I.substCS su c s asig
  substCS su c s (StructureSig m)    = StructureSig $ I.substCS su c s <$> m
  substCS su c s (FunctorSig u)      = FunctorSig $ I.substCS su c s u

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
          t = I.TApp (I.TVar v) $ I.shift 1 ity
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
  | WhilePathElaboration Path I.Type
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
  | SignatureMismatch AbstractSig I.Type
  | TypeMismatch I.Type I.Type
  | NotFunction I.Type
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
  elaborate (TFun t1 t2) = [ (I.TFun t1' t2', I.Mono) | t1' <- elaborate t1 >>= extractMonoType, t2' <- elaborate t2 >>= extractMonoType ]
  elaborate (PathType p) = do
    ssig <- snd <$> elaborate p
    case ssig of
      AtomicType t k -> return (t, k)
      _              -> throwProblem $ NotType p

  elaborate (Package sig) = [ (encode $ norm asig, I.Mono) | asig <- elaborate sig ]

getEnv :: Member (State (I.Env SemanticSig)) r => Eff r (I.Env SemanticSig)
getEnv = get

transaction :: Member (State (I.Env SemanticSig)) r => Eff r a -> Eff r a
transaction e = do
  env <- getEnv
  x <- e
  put env
  return x

insertNothing :: Existential a -> I.Env SemanticSig -> I.Env SemanticSig
insertNothing (Existential (ks, _)) = appEndo $ mconcat $ replicate (length ks + 1) $ Endo I.insertNothing

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
    [ I.Abs (encode $ AtomicTerm t) $ encodeAsTerm $ STerm $ I.App c $ I.Proj (var 0) I.Val
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
      [ I.Abs (encode s) $ I.TmRecord $ coerce $ Map.mapWithKey (\l c -> I.App c $ var 0 `I.Proj` l) o
      | o <- sequence $ Map.intersectionWith (<:) m n
      ]
    | otherwise                                    = throwProblem $ NotSubmap n m

  ssig @ (FunctorSig (Universal (ks1, s :-> a))) <: FunctorSig (Universal (ks2, t :-> b)) = transaction $ do
    updateEnvWithVars ks2
    (c, ts) <- t `match` Existential (ks1, s)
    d <- subst ts a <: b
    return $ I.Abs (encode ssig) $ I.poly ks2 $ I.Abs (encode t) $ I.App d $ I.inst (var 1) (Map.elems ts) `I.App` I.App c (var 0)

  x <: y = throwProblem $ StructuralMismatch x y

lookupCoreType :: Member (Error TypeError) r => Ident -> I.Env SemanticSig -> Eff r (I.Type, Int)
lookupCoreType i e = maybe (throwProblem $ NoSuchIdent i e) return $ I.lookupCoreType (coerce i) e

insertCoreType :: Ident -> I.Type -> I.Env SemanticSig -> I.Env SemanticSig
insertCoreType = I.insertCoreType . coerce

instance Elaboration Expr where
  type Output Expr = (I.Term, I.Type)

  elaborate (IntLit n)   = return (I.IntLit n, I.Int)
  elaborate (Var i)      = get >>= lookupCoreType i >>= \(t, n) -> return (var n, t)
  elaborate (Abs i ty e) = do
    ty1 <- elaborate ty >>= extractMonoType
    (t, ty2) <- transaction $ modify (insertCoreType i ty1) >> elaborate e
    return (I.Abs ty1 t, I.TFun ty1 ty2)
  elaborate (App e1 e2) = do
    (t1, ty1) <- elaborate e1
    (t2, ty2) <- elaborate e2
    case ty1 of
      I.TFun ty11 ty12
        | ty11 .= ty2 -> return (I.App t1 t2, ty12)
        | otherwise   -> throwProblem $ TypeMismatch ty11 ty2
      _ -> throwProblem $ NotFunction ty1
  elaborate (PathExpr p) = elaborate p >>= sequence . bimap (`I.Proj` I.Val) f
    where
      f :: Member (Error TypeError) r => SemanticSig -> Eff r I.Type
      f (AtomicTerm t) = return t
      f _              = throwProblem $ NotTerm p
  elaborate (Pack m sig) = [ (I.App c t, encode asig) | (t, asig') <- elaborate m, asig <- norm <$> elaborate sig, c <- asig' <: asig ]

instance Elaboration Module where
  type Output Module = (I.Term, AbstractSig)

  elaborate (ModuleIdent i) = do
    env <- getEnv
    (ssig, n) <- lookupTypeByIdent i env
    return (var n, existential ssig)

  elaborate (Bindings bs) = transaction $ do
    ps <- traverse f bs
    let asig @ (Existential (ks, _)) = fmap StructureSig $ runIdentity $ foldrM (merge g) (existential mempty) $ snd <$> ps
    let r = fst $ foldr f1 (mempty, 0) $ snd <$> ps
    let t = foldr ($) (pack asig (I.TVar <$> variables ks) $ I.TmRecord r) $ map h ps
    return (t, asig)
      where
        f :: Members Env r => Binding -> Eff r (I.Term, Existential (Map.Map I.Label SemanticSig))
        f b = do
          (t, e) <- elaborate b
          getEnv >>= put . insertNothing e
          updateEnv e
          return (t, e)

        g :: Applicative f => Map.Map I.Label SemanticSig -> Map.Map I.Label SemanticSig -> f (Map.Map I.Label SemanticSig)
        g m1 m2 = pure $ Map.union m2 m1

        h :: (I.Term, Existential (Map.Map I.Label SemanticSig)) -> I.Term -> I.Term
        h (t, e) t0 =
          unpack (StructureSig <$> e) t $
          I.let_ (run $ evalState 0 $ traverse h1 $ Map.toList (fromExistential e)) t0

        h1 :: (I.Label, SemanticSig) -> Eff '[State Int] (I.Term, I.Type)
        h1 (x, y) =
          [ (I.Proj (var n) x, encode y)
          | n <- get
          , _ <- put (n + 1 :: Int)
          ]

        f1 :: Existential (Map.Map I.Label SemanticSig) -> (I.Record I.Term, Int) -> (I.Record I.Term, Int)
        f1 (Existential (ks, m)) (r0, n) =
          let n' = n + Map.size m in
          let w = (`foldMap` Map.keysSet m) $ \l ->
                Endo $ case coerce r0 Map.!? l :: Maybe I.Term of
                  Just _  -> id
                  Nothing -> coerce $ Map.insert l (var n' `I.Proj` l) in
          (appEndo w r0, n' + length ks + 1) -- Note: I.unpack introduces @length ks + 1@ bound variables (@+ 1@ for "let"; see Figure 7).

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

  elaborate (Unpack e sig) = do
    asig <- norm <$> elaborate sig
    (t, ty) <- elaborate e
    if encode asig == ty
      then return (t, asig)
      else throwProblem $ SignatureMismatch asig ty

class Normalize a where
  norm :: a -> a

instance Normalize AbstractSig where
  norm (Existential (ks, ssig)) =
    let ssig' = norm ssig in
    let (ks', s) = sortVars ssig' ks in
      Existential (ks', I.substCS I.Keep 0 s ssig')

instance Normalize SemanticSig where
  norm (AtomicTerm ty)                              = AtomicTerm $ norm ty
  norm s @ (AtomicType _ _)                         = s
  norm (AtomicSig asig)                             = AtomicSig $ norm asig
  norm (StructureSig m)                             = StructureSig $ norm <$> m
  norm (FunctorSig (Universal (ks, ssig :-> asig))) =
    let ssig' = norm ssig in
    let (ks', s) = sortVars ssig' ks in
      FunctorSig $ Universal (ks', I.substCS I.Keep 0 s $ ssig' :-> norm asig)

instance Normalize I.Type where
  norm = id

sortVars :: SemanticSig -> [I.Kind] -> ([I.Kind], Map.Map I.Variable I.Type)
sortVars ssig ks =
  let xs = sortOn (firstAppear ssig . snd) $ zip ks $ coerce [(0 :: Int) ..] in
  let ys = [(k, (I.Variable n, I.TVar v)) | (n, (k, v)) <- zip [0..] xs] in
    (fst <$> ys, Map.fromList $ snd <$> ys)

firstAppear :: SemanticSig -> I.Variable -> First [I.Label]
firstAppear (StructureSig m) v = Map.foldlWithKey (\f l ls -> f <> ((l :) <$> ls)) (First Nothing) $ (`firstAppear` v) <$> m
firstAppear (AtomicType (I.TVar v) _) v0
  | v == v0   = return []
  | otherwise = First Nothing
firstAppear _ _ = First Nothing

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
    let ty = encode $ fromExistential e
    kindOf ty `annotate` WhilePathElaboration (Path m) ty >>= expectMono
    return (unpack e t $ var 0, fromExistential e)
