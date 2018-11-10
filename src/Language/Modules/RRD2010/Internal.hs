{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

  , InternalTypeError(..)

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
import Data.Functor
import qualified Data.Map.Lazy as Map

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
lookupTypeByName name (tenv -> xs) = g $ foldr f (Nothing, 0) xs
  where
    f _ p @ (Just _, _)                                     = p
    f (Just (VarInfo name' x)) (Nothing, n) | name' == name = (Just x, n)
    f _ (_, n)                                              = (Nothing, n + 1)

    g (Just x, n) = Just (x, n)
    g _           = Nothing

insertKind :: Shift a => Kind -> Env a -> Env a
insertKind k e = shift 1 $ e { kenv = k : kenv e }

insertType :: Name -> a -> Env a -> Env a
insertType name x e = e { tenv = return (VarInfo name x) : tenv e }

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
    | c <= coerce v = Map.findWithDefault (shift (- Map.size m) ty) (sub v c) m
    | otherwise     = ty
  substC c m (TFun ty1 ty2) = TFun (substC c m ty1) (substC c m ty2)
  substC c m (TRecord rt)   = TRecord $ substC c m <$> rt
  substC c m (Forall k ty)  = Forall k $ substC (c + 1) m ty
  substC c m (Some k ty)    = Some k $ substC (c + 1) m ty
  substC c m (TAbs k ty)    = TAbs k $ substC (c + 1) m ty
  substC c m (TApp ty1 ty2) = TApp (substC c m ty1) (substC c m ty2)
  substC _ _ Int            = Int

data InternalTypeError
  = NoSuchTypeVariable Variable
  | NotMono Kind
  | KindMismatch Kind Kind
  | ApplicationOfMonotype Type
  deriving (Eq, Show)

expectMono :: Member (Error InternalTypeError) r => Kind -> Eff r ()
expectMono Mono = return ()
expectMono k    = throwError $ NotMono k

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
    Mono -> throwError $ ApplicationOfMonotype t1
    KFun k11 k12
      | k11 == k2 -> return k12
      | otherwise -> throwError $ KindMismatch k11 k2
kindOf Int = return Mono

getKind :: Member (Error InternalTypeError) r => Variable -> Maybe Kind -> Eff r Kind
getKind v = maybe (throwError $ NoSuchTypeVariable v) return
