{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Modules.RRD2010.Internal
  ( Name(..)
  , Label(..)
  , Variable(..)
  , Record(..)
  , Kind(..)
  , Type(..)
  , some
  , forall
  , Term(..)
  , poly
  , inst
  , pack
  , unpack
  , let_

  , kindOf
  , typeOf
  , runTypeOf

  , InternalTypeError(..)
  , Problem(..)
  , fromProblem

  , Shift(..)
  , shift

  , Subst(..)

  -- * Environment
  , Env
  , env
  , VarInfo(..)
  , getName
  , lookupName
  , lookupKind
  , lookupType
  , lookupTypeByName
  , insertKind
  , insertType
  , insertNothing
  ) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Data.Coerce
import Data.Foldable
import Data.Functor
import qualified Data.Map.Lazy as Map
import Data.Monoid

newtype Name = Name String
  deriving (Eq, Ord, Show)

data Label
  = Label Name
  | Val
  | Typ
  | Sig
  deriving (Eq, Ord, Show)

newtype Variable = Variable Int
  deriving (Eq, Ord, Show)

add :: Variable -> Int -> Variable
add (Variable m) n = Variable $ m + n

sub :: Variable -> Int -> Variable
sub (Variable m) n = Variable $ m - n

newtype Record a = Record (Map.Map Label a)
  deriving (Eq, Show, Functor, Foldable, Traversable, Semigroup, Monoid)

data Kind
  = Mono
  | KFun Kind Kind
  deriving (Eq, Show)

data Type
  = TVar Variable
  | TFun Type Type
  | TRecord (Record Type)
  | Forall Kind Type
  | Some Kind Type
  | TAbs Kind Type
  | TApp Type Type
  | Int
  deriving (Eq, Show)

some :: [Kind] -> Type -> Type
some ks ty = foldl (flip Some) ty ks

forall :: [Kind] -> Type -> Type
forall ks ty = foldl (flip Forall) ty ks

data Term
  = Var Variable
  | Abs Type Term
  | App Term Term
  | TmRecord (Record Term)
  | Proj Term Label
  | Poly Kind Term
  | Inst Term Type
  | Pack Type Term Type
  | Unpack Term Term
  | IntLit Int
  deriving (Eq, Show)

poly :: [Kind] -> Term -> Term
poly ks t = foldr Poly t ks

inst :: Term -> [Type] -> Term
inst t ts = foldr (flip Inst) t ts

pack :: [Kind] -> Type -> [Type] -> Term -> Term
pack ks ty ts t = foldr (\(typ1, typ2) tm -> Pack typ1 tm typ2) t $ zip ts $ scanl elimEx (some ks ty) ts

unpack :: Type -> Int -> Term -> Term -> Term
unpack ty n t1 t2 = foldr (\t0 f t -> Unpack t $ f t0) (App $ Abs ty t2) (replicate n $ Var $ Variable 0) t1

let_ :: [(Term, Type)] -> Term -> Term
let_ [] t0             = t0
let_ ((t, ty) : ps) t0 = App (Abs ty $ let_ ps t0) t

data VarInfo a = VarInfo Name a
  deriving (Eq, Show, Functor)

getName :: VarInfo a -> Name
getName (VarInfo name _) = name

fromVarInfo :: VarInfo a -> a
fromVarInfo (VarInfo _ x) = x

data Env a = Env
  { tenv :: [Maybe (VarInfo a)] -- @Nothing@ is used to skip some indices (for translations).
  , kenv :: [Kind]
  }
  deriving (Eq, Show)

env :: Env a
env = Env
  { tenv = []
  , kenv = []
  }

lookupKind :: Variable -> Env a -> Maybe Kind
lookupKind (Variable n) (kenv -> xs)
  | 0 <= n && n < length xs = return $ xs !! n
  | otherwise               = Nothing

lookupType :: Variable -> Env a -> Maybe (VarInfo a)
lookupType (Variable n) (tenv -> xs)
  | 0 <= n && n < length xs = xs !! n
  | otherwise               = Nothing

lookupName :: Variable -> Env a -> Maybe Name
lookupName v e = getName <$> lookupType v e

lookupTypeByName :: Name -> Env a -> Maybe (a, Int)
lookupTypeByName name (tenv -> xs) = g $ foldl f (Nothing, 0) xs
  where
    f p @ (Just _, _) _                                     = p
    f (Nothing, n) (Just (VarInfo name' x)) | name' == name = (Just x, n)
    f (_, n) _                                              = (Nothing, n + 1)

    g (Just x, n) = Just (x, n)
    g _           = Nothing

insertKind :: Shift a => Kind -> Env a -> Env a
insertKind k e = shift 1 $ e { kenv = k : kenv e }

insertType :: Name -> a -> Env a -> Env a
insertType name x e = e { tenv = return (VarInfo name x) : tenv e }

insertTypeWithoutName :: a -> Env a -> Env a
insertTypeWithoutName = insertType $ Name ""

insertNothing :: Env a -> Env a
insertNothing e = e { tenv = Nothing : tenv e }

shift :: Shift a => Int -> a -> a
shift = shiftAbove 0

class Shift a where
  shiftAbove :: Int -> Int -> a -> a

instance Shift Type where
  shiftAbove c0 d = walk c0
    where
      walk :: Int -> Type -> Type
      walk c t @ (TVar v)
        | c <= coerce v = TVar $ add v d
        | otherwise     = t
      walk c (TFun t1 t2) = TFun (walk c t1) (walk c t2)
      walk c (TRecord r)  = TRecord $ walk c <$> r
      walk c (Forall k t) = Forall k $ walk (c + 1) t
      walk c (Some k t)   = Some k $ walk (c + 1) t
      walk c (TAbs k t)   = TAbs k $ walk (c + 1) t
      walk c (TApp t1 t2) = TApp (walk c t1) (walk c t2)
      walk _ Int          = Int

instance Shift a => Shift (Maybe a) where
  shiftAbove c d = fmap $ shiftAbove c d

instance Shift a => Shift (Env a) where
  shiftAbove c0 d e = e { tenv = shiftAbove c0 d <$> tenv e }

instance Shift a => Shift (VarInfo a) where
  shiftAbove c0 d info = shiftAbove c0 d <$> info

instance Shift a => Shift (Map.Map k a) where
  shiftAbove c0 d m = shiftAbove c0 d <$> m

elimEx :: Type -> Type -> Type
elimEx (Some _ t) x = substC 0 (Map.singleton (Variable 0) x) t
elimEx _ _          = error "unexpected error"

class Subst a where
  -- Assumes @Map.keys m == coerce [0 .. Map.size m - 1]@ where @m@ ranges over the second argument of @substC@.
  substC :: Int -> Map.Map Variable Type -> a -> a

instance Subst Type where
  substC c m ty @ (TVar v)
    | c <= coerce v = maybe (shift (- Map.size m) ty) (shift c) $ Map.lookup (sub v c) m
    | otherwise     = ty
  substC c m (TFun ty1 ty2) = TFun (substC c m ty1) (substC c m ty2)
  substC c m (TRecord rt)   = TRecord $ substC c m <$> rt
  substC c m (Forall k ty)  = Forall k $ substC (c + 1) m ty
  substC c m (Some k ty)    = Some k $ substC (c + 1) m ty
  substC c m (TAbs k ty)    = TAbs k $ substC (c + 1) m ty
  substC c m (TApp ty1 ty2) = TApp (substC c m ty1) (substC c m ty2)
  substC _ _ Int            = Int

data InternalTypeError = InternalTypeError [Reason] Problem
  deriving (Eq, Show)

data Problem
  = NoSuchTypeVariable Variable
  | NoSuchVariable Variable
  | NoSuchLabel Label
  | NotMono Kind
  | KindMismatch Kind Kind
  | TypeMismatch Type Type
  | ApplicationOfMonotype Type
  | NotFunction Type
  | NotRecord Type
  | NotForall Type
  | NotSome Type
  deriving (Eq, Show)

data Reason
  = TermApp Term Term
  deriving (Eq, Show)

fromProblem :: Problem -> InternalTypeError
fromProblem = InternalTypeError []

throwProblem :: Member (Error InternalTypeError) r => Problem -> Eff r a
throwProblem = throwError . fromProblem

addReason :: Reason -> InternalTypeError -> InternalTypeError
addReason r (InternalTypeError rs p) = InternalTypeError (r : rs) p

annotate :: Member (Error InternalTypeError) r => Eff r a -> Reason -> Eff r a
annotate x r = x `catchError` (throwError . addReason r)

expectMono :: Member (Error InternalTypeError) r => Kind -> Eff r ()
expectMono Mono = return ()
expectMono k    = throwProblem $ NotMono k

-- Assumes that each variable in the environment is assigned a monotype.
kindOf :: forall a. Shift a => Type -> Eff '[Reader (Env a), Error InternalTypeError] Kind
kindOf (TVar v)     = asks (\(e :: Env a) -> lookupKind v e) >>= getKind v
kindOf (TFun t1 t2) = kindOf t1 >>= expectMono >> (kindOf t2 >>= expectMono) $> Mono
kindOf (TRecord r)  = mapM_ (expectMono <=< kindOf) r $> Mono
kindOf (Forall k t) = local (\(e :: Env a) -> insertKind k e) $ (kindOf t >>= expectMono) $> Mono
kindOf (Some k t)   = local (\(e :: Env a) -> insertKind k e) $ (kindOf t >>= expectMono) $> Mono
kindOf (TAbs k t)   = local (\(e :: Env a) -> insertKind k e) $ KFun k <$> kindOf t
kindOf (TApp t1 t2) = do
  k1 <- kindOf t1
  k2 <- kindOf t2
  case k1 of
    Mono -> throwProblem $ ApplicationOfMonotype t1
    KFun k11 k12
      | k11 == k2 -> return k12
      | otherwise -> throwProblem $ KindMismatch k11 k2
kindOf Int = return Mono

getKind :: Member (Error InternalTypeError) r => Variable -> Maybe Kind -> Eff r Kind
getKind v = maybe (throwProblem $ NoSuchTypeVariable v) return

runTypeOf :: Env Type -> Term -> Either InternalTypeError Type
runTypeOf e t = run $ runError $ runReader e $ typeOf t

typeOf :: Members '[Reader (Env Type), Error InternalTypeError] r => Term -> Eff r Type
typeOf (Var v)     = ask >>= getType v . fmap fromVarInfo . lookupType v
typeOf (Abs ty t)  = local (insertTypeWithoutName ty) $ TFun ty <$> typeOf t
typeOf (App t1 t2) = do
  ty1 <- reduce <$> typeOf t1
  ty2 <- reduce <$> typeOf t2
  withTFun ty1 $ \ty11 ty12 ->
    if ty11 == ty2
      then return ty12
      else throwError $ InternalTypeError [TermApp t1 t2] $ TypeMismatch ty11 ty2
typeOf (TmRecord r) = TRecord <$> mapM typeOf r
typeOf (Proj t l)   = do
  ty <- reduce <$> typeOf t
  withTRecord ty $ \r ->
    case coerce r Map.!? l of
      Just ty1 -> return ty1
      Nothing  -> throwProblem $ NoSuchLabel l
typeOf (Poly k t)       = local (\(e :: Env Type) -> insertKind k e) $ Forall k <$> typeOf t
typeOf (Inst t ty)      = reduce <$> typeOf t >>= \ty1 -> withForall ty1 $ \k ty2 -> expect ty k $> substC 0 (Map.singleton (Variable 0) ty) ty2
typeOf (Pack ty1 t ty2) = (expect ty2 Mono >>) $ withSome ty2 $ \k ty3 -> expect ty1 k >> expect t (substC 0 (Map.singleton (Variable 0) ty1) ty3) $> Some k ty3
typeOf (Unpack t1 t2)   = typeOf t1 >>= \ty1 -> [ty2 | ty2 <- withSome ty1 $ \k ty -> local (insertTypeWithoutName ty . insertKind k) $ typeOf t2, _ <- expect ty2 Mono]
typeOf (IntLit _)       = return Int

kindOf' :: Members '[Reader (Env Type), Error InternalTypeError] r => Type -> Eff r Kind
kindOf' ty = ask >>= \(e :: Env Type) -> either throwError return $ run $ runError $ runReader e $ kindOf ty

getType :: Member (Error InternalTypeError) r => Variable -> Maybe Type -> Eff r Type
getType v = maybe (throwProblem $ NoSuchVariable v) return

withTFun :: Members '[Reader (Env Type), Error InternalTypeError] r => Type -> (Type -> Type -> Eff r Type) -> Eff r Type
withTFun (TFun ty1 ty2) f = f ty1 ty2
withTFun ty _             = throwProblem $ NotFunction ty

withTRecord :: Members '[Reader (Env Type), Error InternalTypeError] r => Type -> (Record Type -> Eff r Type) -> Eff r Type
withTRecord (TRecord r) f = f r
withTRecord ty _          = throwProblem $ NotRecord ty

withForall :: Members '[Reader (Env Type), Error InternalTypeError] r => Type -> (Kind -> Type -> Eff r Type) -> Eff r Type
withForall (Forall k ty) f = f k ty
withForall ty _          = throwProblem $ NotForall ty

withSome :: Members '[Reader (Env Type), Error InternalTypeError] r => Type -> (Kind -> Type -> Eff r Type) -> Eff r Type
withSome (Some k ty) f = f k ty
withSome ty _          = throwProblem $ NotSome ty

class Expect a where
  type Expected a
  expect :: Members '[Reader (Env Type), Error InternalTypeError] r => a -> Expected a -> Eff r ()

instance Expect Type where
  type Expected Type = Kind
  expect ty k0 = do
    k <- kindOf' ty
    when (k /= k0) $
      throwProblem $ KindMismatch k k0

instance Expect Term where
  type Expected Term = Type
  expect t ty0 = do
    ty <- typeOf t
    when (ty /= ty0) $
      throwProblem $ TypeMismatch ty ty0

reduce :: Type -> Type
reduce t @ (TVar _) = t
reduce (TFun t1 t2) = TFun (reduce t1) $ reduce t2
reduce (TRecord r)  = TRecord $ reduce <$> r
reduce (Forall k t) = Forall k $ reduce t
reduce (Some k t)   = Some k $ reduce t
reduce (TAbs k t)   =
  case reduce t of
    TApp t2 (TVar (Variable 0))
      | getAll $ run $ runReader 0 $ eta t2 -> shift (-1) t2
    t1 -> TAbs k t1
reduce (TApp t1 t2) =
  let t21 = reduce t2 in
  case reduce t1 of
    (TAbs _ t0) -> substC 0 (Map.singleton (Variable 0) t21) $ t0
    t11         -> TApp t11 t21
reduce Int          = Int

eta :: Type -> Eff '[Reader Int] All
eta (TVar v)     = [All $ v /= Variable n | n <- ask]
eta (TFun t1 t2) = (<>) <$> eta t1 <*> eta t2
eta (TRecord r)  = fold <$> mapM eta r
eta (Forall _ t) = local inc $ eta t
eta (Some _ t)   = local inc $ eta t
eta (TAbs _ t)   = local inc $ eta t
eta (TApp t1 t2) = (<>) <$> eta t1 <*> eta t2
eta Int          = return $ All True

inc :: Int -> Int
inc = (+ 1)
