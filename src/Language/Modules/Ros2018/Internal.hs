{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Modules.Ros2018.Internal
  (
  -- * Objects
    Variable
  , variable
  , Generated
  , generated
  , Name
  , name
  , Label
  , label

  -- * Records
  , Record
  , record

  -- * Syntax
  , Kind(..)
  , Type(..)
  , BaseType(..)
  , Term(..)
  , Literal(..)

  -- * Useful functions
  , tvar

  -- * Kinding
  , Kinded(..)

  -- * Typing
  , Typed(..)
  , whTypeOf

  -- * Type equivalence and reduction
  , equal
  , reduce

  -- * Type substitution
  , substTop

  -- * Environments
  , Env
  , emptyEnv
  , insertType
  , insertValue
  , lookupType
  , lookupValueByName

  -- * Annotation
  , Annotated(..)

  -- * Errors
  , EnvError(..)
  , TypeEquivError(..)
  , KindError(..)
  , TypeError(..)

  -- * Failure
  , Failure(..)
  , Evidence(..)
  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Data.Coerce
import Data.Functor
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Map.Merge.Lazy
import Data.Monoid
import GHC.Generics

import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Shift

newtype Label = Label String
  deriving (Eq, Ord, Show)

label :: String -> Label
label = coerce

instance Display Label where
  display (Label s) = s

newtype Variable = Variable { getVariable :: Int }
  deriving (Eq, Ord, Show)
  deriving Shift via IndexedVariable

variable :: Int -> Variable
variable = coerce

instance Display Variable where
  display (Variable n) = "v[" ++ show n ++ "]"

newtype Generated = Generated Int
  deriving (Eq, Show)

instance Display Generated where
  display (Generated n) = "g" ++ show n

generated :: Int -> Generated
generated = coerce

newtype Name = Name String
  deriving (Eq, Ord, Show)

name :: String -> Name
name = coerce

instance Display Name where
  display (Name s) = s

newtype Record a = Record { getRecord :: Map.Map Label a }
  deriving (Eq, Show)
  deriving Functor

instance Display a => Display (Record a) where
  displaysPrec _ (Record m) =
    let xs = map (\(l, x) -> displays l . showString ": " . displays x) $ Map.toList m in
    let ys = intersperse (showString ", ") xs in
      showString "{" . appEndo (mconcat $ coerce ys) . showString "}"

instance DisplayName a => DisplayName (Record a) where
  displaysWithName _ (Record m) =
    let xs = map (\(l, x) -> displays l . showString ": " . displaysWithName 0 x) $ Map.toList m in
    let ys = intersperse (showString ", ") xs in
      showString "{" . appEndo (mconcat $ coerce ys) . showString "}"

instance Shift a => Shift (Record a) where
  shiftAbove c d = fmap $ shiftAbove c d

record :: [(Label, a)] -> Record a
record = Record . Map.fromList

data Kind
  = Base
  | KFun Kind Kind
  deriving (Eq, Show)
  deriving Shift via Fixed Kind

instance Display Kind where
  displaysPrec _ Base         = showString "*"
  displaysPrec n (KFun k1 k2) = showParen (4 <= n) $ displaysPrec 4 k1 . showString " -> " . displaysPrec 3 k2

data BaseType
  = Bool
  | Int
  | Char
  deriving (Eq, Show)
  deriving Shift via Fixed BaseType

instance Display BaseType where
  display Bool = "bool"
  display Int  = "int"
  display Char = "char"

data Type
  = BaseType BaseType
  | TVar Variable
  | TFun Type Type
  | TRecord (Record Type)
  | Forall Kind Type
  | Some Kind Type
  | TAbs Kind Type
  | TApp Type Type
  deriving (Eq, Show)
  deriving Generic

instance Display Type where
  displaysPrec n (BaseType b)   = displaysPrec n b
  displaysPrec n (TVar v)       = displaysPrec n v
  displaysPrec n (TFun ty1 ty2) = showParen (4 <= n) $ displaysPrec 4 ty1 . showString " -> " . displaysPrec 3 ty2
  displaysPrec n (TRecord r)    = displaysPrec n r
  displaysPrec n (Forall k ty)  = showParen (4 <= n) $ showChar '∀' . displays k . showString ". " . displays ty
  displaysPrec n (Some k ty)    = showParen (4 <= n) $ showChar '∃' . displays k . showString ". " . displays ty
  displaysPrec n (TAbs k ty)    = showParen (4 <= n) $ showChar 'λ' . displays k . showString ". " . displays ty
  displaysPrec n (TApp ty1 ty2) = showParen (5 <= n) $ displaysPrec 4 ty1 . showString " " . displaysPrec 5 ty2

instance DisplayName Type where
  displaysWithName n (BaseType b)   = displaysPrec n b
  displaysWithName _ (TVar v)       = displayTypeVariable $ getVariable v
  displaysWithName n (TFun ty1 ty2) = showParen (4 <= n) $ displaysWithName 4 ty1 . showString " -> " . displaysWithName 3 ty2
  displaysWithName n (TRecord r)    = displaysWithName n r
  displaysWithName n (Forall k ty)  =
    let ?nctx = newType in
      showParen (4 <= n) $ showChar '∀' . displayTypeVariable 0 . showString " : " . displays k . showString ". " . displaysWithName 0 ty
  displaysWithName n (Some k ty)    =
    let ?nctx = newType in
      showParen (4 <= n) $ showChar '∃' . displayTypeVariable 0 . showString " : " . displays k . showString ". " . displaysWithName 0 ty
  displaysWithName n (TAbs k ty)    =
    let ?nctx = newType in
      showParen (4 <= n) $ showChar 'λ' . displayTypeVariable 0 . showString " : " . displays k . showString ". " . displaysWithName 0 ty
  displaysWithName n (TApp ty1 ty2) = showParen (5 <= n) $ displaysWithName 4 ty1 . showString " " . displaysWithName 5 ty2

instance Shift Type where
  shiftAbove c d (Forall k ty) = Forall k $ shiftAbove (c + 1) d ty
  shiftAbove c d (Some k ty)   = Some k $ shiftAbove (c + 1) d ty
  shiftAbove c d (TAbs k ty)   = TAbs k $ shiftAbove (c + 1) d ty
  shiftAbove c d ty            = to $ gShiftAbove c d $ from ty

tvar :: Int -> Type
tvar = TVar . variable

some :: [Kind] -> Type -> Type
some ks ty = foldl (flip Some) ty ks

data Literal
  = LBool Bool
  | LInt Int
  | LChar Char
  deriving (Eq, Show)

instance Display Literal where
  display (LBool b)
    | b         = "true"
    | otherwise = "false"
  display (LInt n)   = show n
  display (LChar ch) = show ch

data Term
  = Lit Literal
  | Var Variable
  | GVar Generated
  | Abs Type Term
  | App Term Term
  | TmRecord (Record Term)
  | Proj Term Label
  | Poly Kind Term
  | Inst Term Type
  | Pack Term [Type] [Kind] Type
  | Unpack (Maybe Generated) Term Int Term
  -- To make debug easy.
  | Let [Term] Term
  deriving (Eq, Show)

instance Display Term where
  displaysPrec _ (Lit l)             = displays l
  displaysPrec _ (Var v)             = displays v
  displaysPrec _ (GVar g)            = displays g
  displaysPrec n (Abs ty t)          = showParen (4 <= n) $ showChar 'λ' . displays ty . showString ". " . displays t
  displaysPrec n (App t1 t2)         = showParen (5 <= n) $ displaysPrec 4 t1 . showString " " . displaysPrec 5 t2
  displaysPrec n (TmRecord r)        = displaysPrec n r
  displaysPrec _ (Proj t l)          = displaysPrec 5 t . showChar '.' . displays l
  displaysPrec n (Poly k t)          = showParen (4 <= n) $ showChar 'Λ' . displays k . showString ". " . displays t
  displaysPrec n (Inst t ty)         = showParen (5 <= n) $ displaysPrec 4 t . showString " [" . displays ty . showChar ']'
  displaysPrec n (Pack t tys ks ty)  = showParen (2 <= n) $ showString "pack [" . displayTypesRev tys . showString "; " . displays t . showString "] as " . displays (some ks ty)
  displaysPrec n (Unpack mg t1 m t2) =
    case mg of
      Nothing -> showParen (4 <= n) $ showString "unpack [" . shows m . showString "] = " . displays t1 . showString " in " . displays t2
      Just g  -> showParen (4 <= n) $ showString "unpack [" . displays g . showString ", " . shows m . showString "] = " . displays t1 . showString " in " . displays t2
  displaysPrec n (Let ts t)          = showParen (4 <= n) $ showString "let " . displaySemi ts . showString " in " . displays t

instance DisplayName Term where
  displaysWithName _ (Lit l)      = displays l
  displaysWithName _ (Var v)      = displayVariable $ getVariable v
  displaysWithName _ (GVar g)     = displays g
  displaysWithName n (Abs ty t)   =
    let ?nctx = newValue in
      showParen (4 <= n) $ showChar 'λ' . displayVariable 0 . showString " : " . displaysWithName 0 ty . showString ". " . displaysWithName 0 t
  displaysWithName n (App t1 t2)  = showParen (5 <= n) $ displaysWithName 4 t1 . showString " " . displaysWithName 5 t2
  displaysWithName n (TmRecord r) = displaysWithName n r
  displaysWithName _ (Proj t l)   = displaysWithName 5 t . showChar '.' . displays l
  displaysWithName n (Poly k t)   =
    let ?nctx = newType in
      showParen (4 <= n) $ showChar 'Λ' . displayTypeVariable 0 . showString " : " . displays k . showString ". " . displaysWithName 0 t
  displaysWithName n (Inst t ty)        = showParen (5 <= n) $ displaysWithName 4 t . showString " [" . displaysWithName 0 ty . showChar ']'
  displaysWithName n (Pack t tys ks ty) = showParen (2 <= n) $ showString "pack [" . displayTypesRevWithName tys . showString "; " . displaysWithName 0 t . showString "] as " . displaysWithName 0 (some ks ty)
  displaysWithName n (Unpack mg t1 m t2) =
    case mg of
      Nothing ->
        let f = displaysWithName 0 t1 in
        let ?nctx = newValue in
        let ?nctx = newTypes m in
          showParen (4 <= n) $ showString "unpack [" . displayVariable 0 . showString ", " . displayTypeVariables m . showString "] = " . f . showString " in " . displaysWithName 0 t2
      Just g ->
        let f = displaysWithName 0 t1 in
        let ?nctx = newTypes m in
          showParen (4 <= n) $ showString "unpack [" . displays g . showString ", " . displayTypeVariables m . showString "] = " . f . showString " in " . displaysWithName 0 t2
  displaysWithName n (Let ts t) =
    let fs = displaySemiWithName ts in
    let ?nctx = newValues $ length ts in
    let f = displaysWithName 0 t in
      showParen (4 <= n) $ showString "let " . appEndo (mconcat $ coerce $ intersperse (showString "; ") $ fst $ foldr (\f (fs, i) -> (displayVariable i . showString " = " . f : fs, i + 1)) ([], 0) fs) . showString " in " . f

displayTypeVariables :: (?nctx :: NameContext) => Int -> ShowS
displayTypeVariables 0 = id
displayTypeVariables 1 = displayTypeVariable 0
displayTypeVariables n = displayTypeVariable (n - 1) . showString ".." . displayTypeVariable 0

displaySemi :: Display a => [a] -> ShowS
displaySemi = appEndo . mconcat . coerce . intersperse (showString "; ") . map displays

displaySemiWithName :: (DisplayName a, ?nctx :: NameContext) => [a] -> [ShowS]
displaySemiWithName = map $ displaysWithName 0

displayTypesRev :: [Type] -> ShowS
displayTypesRev = appEndo . getDual . mconcat . coerce . intersperse (showString ", ") . map displays

displayTypesRevWithName :: (?nctx :: NameContext) => [Type] -> ShowS
displayTypesRevWithName = appEndo . getDual . mconcat . coerce . intersperse (showString ", ") . map (displaysWithName 0)

data Env f ty = Env
  { tenv :: [f Kind]
  , venv :: [ty]
  , nmap :: Map.Map Name Int
  , tempVenv :: [ty]
  }

emptyEnv :: Env f ty
emptyEnv = Env
  { tenv = []
  , venv = []
  , nmap = mempty
  , tempVenv = []
  }

class Annotated f where
  extract :: f a -> a
  unannotated :: a -> f a

data Failure = forall a. Failure a (Evidence a) (a -> String)

data Evidence a where
  EvidEnv :: Evidence EnvError
  EvidTypeEquiv :: Evidence TypeEquivError
  EvidKind :: Evidence KindError
  EvidType :: Evidence TypeError

class Display a => SpecificError a where
  evidence :: Evidence a

fromSpecific :: SpecificError a => a -> Failure
fromSpecific x = Failure x evidence display

throw :: (Member (Error Failure) r, SpecificError a) => a -> Eff r b
throw = throwError . fromSpecific

data EnvError
  = UnboundName Name
  | UnboundVariable Variable
  | UnboundGeneratedVariable Generated
  | UnboundTypeVariable Variable
  deriving (Eq, Show)

instance Display EnvError where
  display (UnboundName name)           = "unbound name: " ++ display name
  display (UnboundVariable v)          = "unbound variable: " ++ display v
  display (UnboundGeneratedVariable g) = "unbound generated variable: " ++ display g
  display (UnboundTypeVariable v)      = "unbound type variable: " ++ display v

instance SpecificError EnvError where
  evidence = EvidEnv

insertType :: (Shift ty, ?env :: Env f ty) => f Kind -> Env f ty
insertType k = ?env
  { tenv = k : tenv ?env
  , venv = shift 1 $ venv ?env
  , tempVenv = shift 1 $ tempVenv ?env
  }

insertValue :: (?env :: Env f ty) => Name -> ty -> Env f ty
insertValue name ty = ?env
  { venv = ty : venv ?env
  , nmap = Map.insert name (length (venv ?env) + 1) $ nmap ?env
  }

insertValueWithoutName :: (?env :: Env f ty) => ty -> Env f ty
insertValueWithoutName ty = ?env
  { venv = ty : venv ?env
  }

lookupType :: (Member (Error Failure) r, ?env :: Env f ty) => Variable -> Eff r (f Kind)
lookupType (Variable n) = do
  case tenv ?env of
    xs | 0 <= n && n < length xs -> return $ xs !! n
    _                            -> throw $ UnboundTypeVariable $ Variable n

lookupValueByName :: (Member (Error Failure) r, ?env :: Env f ty) => Name -> Eff r ty
lookupValueByName name = do
  n <- maybe (throw $ UnboundName name) return $ Map.lookup name $ nmap ?env
  lookupValue $ Variable $ length (venv ?env) - n

lookupValue :: (Member (Error Failure) r, ?env :: Env f ty) => Variable -> Eff r ty
lookupValue (Variable n) = do
  case venv ?env of
    xs | 0 <= n && n < length xs -> return $ xs !! n
    _                            -> throw $ UnboundVariable $ Variable n

lookupTempValue :: (Member (Error Failure) r, ?env :: Env f ty) => Generated -> Eff r ty
lookupTempValue (Generated n) = do
  case tempVenv ?env of
    xs | 0 <= n && n < length xs -> return $ xs !! n
    _                            -> throw $ UnboundGeneratedVariable $ Generated n

data TypeEquivError
  = StructurallyInequivalent Type Type
  | KindMismatch Kind Kind
  | MissingLabelL Label (Record Type) (Record Type)
  | MissingLabelR Label (Record Type) (Record Type)
  deriving (Eq, Show)

instance Display TypeEquivError where
  display (StructurallyInequivalent ty1 ty2) = "structurally inequivalent types: " ++ display ty1 ++ " and " ++ display ty2
  display (KindMismatch k1 k2)               = "kind mismatch: " ++ display k1 ++ " and " ++ display k2
  display (MissingLabelL l r1 r2)            = "comparing " ++ display r1 ++ " and " ++ display r2 ++ ": missing label " ++ display l ++ "in left"
  display (MissingLabelR l r1 r2)            = "comparing " ++ display r1 ++ " and " ++ display r2 ++ ": missing label " ++ display l ++ "in right"

instance SpecificError TypeEquivError where
  evidence = EvidTypeEquiv

-- Assumes well-kindness of input types.
equal :: (Shift ty, Annotated f, Member (Error Failure) r, ?env :: Env f ty) => Type -> Type -> Kind -> Eff r ()
equal ty1 ty2 Base         = void $ strEquiv (reduce ty1) (reduce ty2)
equal ty1 ty2 (KFun k1 k2) =
  let ?env = insertType $ unannotated k1 in
    equal (TApp (shift 1 ty1) $ tvar 0) (TApp (shift 1 ty2) $ tvar 0) k2

strEquiv :: (Shift ty, Annotated f, Member (Error Failure) r, ?env :: Env f ty) => Type -> Type -> Eff r Kind
strEquiv ty1 @ (BaseType b1) ty2 @ (BaseType b2)
  | b1 == b2  = return Base -- Assumes base types have the base kind.
  | otherwise = throw $ StructurallyInequivalent ty1 ty2
strEquiv ty1 @ (TVar v1) ty2 @ (TVar v2)
  | v1 == v2  = extract <$> lookupType v1
  | otherwise = throw $ StructurallyInequivalent ty1 ty2
strEquiv (TFun ty11 ty12) (TFun ty21 ty22) = do
  equal ty11 ty21 Base
  equal ty12 ty22 Base
  return Base
strEquiv (TRecord r1 @ (Record m1)) (TRecord r2 @ (Record m2)) = do
  let f l _ = throw $ MissingLabelL l r1 r2
  let g l _ = throw $ MissingLabelR l r1 r2
  let h _ ty1 ty2 = equal ty1 ty2 Base
  _ <- mergeA (traverseMissing f) (traverseMissing g) (zipWithAMatched h) m1 m2
  return Base
strEquiv (Forall k1 ty1) (Forall k2 ty2)
  | k1 == k2  = let ?env = insertType $ unannotated k1 in equal ty1 ty2 Base $> Base
  | otherwise = throw $ KindMismatch k1 k2
strEquiv (Some k1 ty1) (Some k2 ty2)
  | k1 == k2  = let ?env = insertType $ unannotated k1 in equal ty1 ty2 Base $> Base
  | otherwise = throw $ KindMismatch k1 k2
strEquiv (TApp ty11 ty12) (TApp ty21 ty22) = do
  k <- strEquiv ty11 ty21
  case k of
    KFun k1 k2 -> do
      equal ty12 ty22 k1
      return k2
    Base -> error $ "unexpected base kind, which is kind of " ++ display ty11
strEquiv ty1 ty2 = throw $ StructurallyInequivalent ty1 ty2

reduce :: Type -> Type
reduce (TApp ty1 ty2) = reduce' (reduce ty1) ty2
reduce ty             = ty

reduce' :: Type -> Type -> Type
reduce' (TAbs _ ty1) ty2 = reduce $ substTop ty2 ty1
reduce' ty1 ty2          = TApp ty1 ty2

newtype Subst = Subst (Map.Map Variable Type)
  deriving (Eq, Show)

instance Shift Subst where
  shiftAbove c d (Subst m) = Subst $ shiftAbove c d <$> m

lookupSubst :: Variable -> Subst -> Maybe Type
lookupSubst v (Subst m) = Map.lookup v m

class Substitution a where
  apply :: Subst -> a -> a

instance Substitution Type where
  apply _ ty @ (BaseType _) = ty
  apply s ty @ (TVar v)     = fromMaybe ty $ lookupSubst v s
  apply s (TFun ty1 ty2)    = apply s ty1 `TFun` apply s ty2
  apply s (TRecord r)       = TRecord $ apply s r
  apply s (Forall k ty)     = Forall k $ apply (shift 1 s) ty
  apply s (Some k ty)       = Some k $ apply (shift 1 s) ty
  apply s (TAbs k ty)       = TAbs k $ apply (shift 1 s) ty
  apply s (TApp ty1 ty2)    = apply s ty1 `TApp` apply s ty2

instance Substitution a => Substitution (Record a) where
  apply s (Record m) = Record $ apply s <$> m

substTop :: Type -> Type -> Type
substTop by = shift (-1) . subst 0 (shift 1 by)

subst :: Int -> Type -> Type -> Type
subst n by = apply $ Subst $ Map.singleton (Variable n) by

class Kinded a where
  kindOf :: (Shift ty, Annotated f, Member (Error Failure) r, ?env :: Env f ty) => a -> Eff r Kind

data KindError
  = NotBase Kind
  | NotFunctionKind Kind
  | KindMismatch_ Kind Kind
  deriving (Eq, Show)

instance Display KindError where
  display (NotBase k)          = "not base kind: " ++ show k
  display (NotFunctionKind k)  = "not function kind: " ++ show k
  display (KindMismatch_ k1 k2) = "kind mismatch: " ++ show k1 ++ " and " ++ show k2

instance SpecificError KindError where
  evidence = EvidKind

instance Kinded BaseType where
  kindOf _ = return Base

instance Kinded a => Kinded (Record a) where
  kindOf (Record m) = mapM_ mustBeBase m $> Base

instance Kinded Type where
  kindOf (BaseType b)   = kindOf b
  kindOf (TVar v)       = extract <$> lookupType v
  kindOf (TFun ty1 ty2) = do
    mustBeBase ty1
    mustBeBase ty2
    return Base
  kindOf (TRecord r)    = kindOf r
  kindOf (Forall k ty)  = let ?env = insertType $ unannotated k in mustBeBase ty $> Base
  kindOf (Some k ty)    = let ?env = insertType $ unannotated k in mustBeBase ty $> Base
  kindOf (TAbs k ty)    = let ?env = insertType $ unannotated k in (KFun k) <$> kindOf ty
  kindOf (TApp ty1 ty2) = do
    k1 <- kindOf ty1
    k2 <- kindOf ty2
    case k1 of
      KFun k11 k12
        | k11 == k2 -> return k12
        | otherwise -> throw $ KindMismatch_ k11 k2
      Base -> throw $ NotFunctionKind k1

mustBeBase :: (Kinded a, Shift ty, Annotated f, Member (Error Failure) r, ?env :: Env f ty) => a -> Eff r ()
mustBeBase x = do
  k <- kindOf x
  case k of
    Base     -> return ()
    KFun _ _ -> throw $ NotBase k

class Typed a where
  typeOf :: (Annotated f, Member (Error Failure) r, ?env :: Env f Type) => a -> Eff r Type

whTypeOf :: (Typed a, Annotated f, Member (Error Failure) r, ?env :: Env f Type) => a -> Eff r Type
whTypeOf x = reduce <$> typeOf x

data TypeError
  = NotFunction Type
  | TypeMismatch Type Type
  deriving (Eq, Show)

instance Display TypeError where
  display (NotFunction ty)       = "not function type: " ++ display ty
  display (TypeMismatch ty1 ty2) = "type mismatch: " ++ display ty1 ++ " and " ++ display ty2

instance SpecificError TypeError where
  evidence = EvidType

instance Typed Literal where
  typeOf (LBool _) = return $ BaseType Bool
  typeOf (LInt _)  = return $ BaseType Int
  typeOf (LChar _) = return $ BaseType Char

-- Invariant: `typeOf` always returns, if any, a type which has the base kind.
instance Typed Term where
  typeOf (Lit l)    = typeOf l
  typeOf (Var v)    = lookupValue v
  typeOf (GVar g)   = lookupTempValue g
  typeOf (Abs ty t) = mustBeBase ty >> let ?env = insertValueWithoutName ty in (TFun ty) <$> typeOf t
  typeOf (App t1 t2) = do
    ty1 <- whTypeOf t1
    ty2 <- typeOf t2
    case ty1 of
      TFun ty11 ty12 -> equal ty11 ty2 Base $> ty12
      _              -> throw $ NotFunction ty1
