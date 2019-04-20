{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Modules.Ros2018.Internal
  (
  -- * Objects
    Variable
  , variable
  , (-:)
  , Generated
  , generated
  , Name
  , name
  , toName
  , Label
  , label
  , toLabel

  -- * Records
  , Record(..)
  , record
  , intersection
  , toList
  , labels
  , foldMapIntersection
  , iter
  , projRecord
  , updateRecord

  -- * Syntax
  , Kind(..)
  , Type(..)
  , BaseType(..)
  , Term(..)
  , Literal(..)

  -- * Useful functions
  , tvar
  , some
  , forall
  , tabs
  , var
  , poly
  , inst
  , pack
  , unpack

  -- * Kinding
  , Kinded(..)

  -- * Typing
  , Typed(..)
  , whTypeOf
  , typeOfLiteral
  , typecheck

  -- * Type equivalence and reduction
  , equal
  , equalType
  , reduce

  -- * Type substitution
  , Subst
  , SubstP
  , Substitution(..)
  , substTop
  , lookupSubst

  -- * Environments
  , Env
  , emptyEnv
  , tenvLen
  , insertType
  , insertTypes
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

  -- * Proxy
  , VProxy
  ) where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Data.Coerce
import Data.Foldable (fold)
import Data.Functor
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Map.Merge.Lazy
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Exts
import GHC.Generics

import Language.Modules.Ros2018.Display
import qualified Language.Modules.Ros2018.Ftv as Ftv
import Language.Modules.Ros2018.Shift

newtype Label = Label T.Text
  deriving (Eq, Ord, Show)

label :: T.Text -> Label
label = coerce

instance Display Label where
  display (Label s) = T.unpack s

toName :: Label -> Name
toName = coerce

newtype Variable = Variable { getVariable :: Int }
  deriving (Eq, Ord, Show)
  deriving Shift via IndexedVariable

variable :: Int -> Variable
variable = coerce

instance Display Variable where
  display (Variable n) = "v[" ++ show n ++ "]"

(-:) :: Variable -> Variable -> Variable
Variable m -: Variable n = Variable $ m - n

newtype Generated = Generated Int
  deriving (Eq, Show)

instance Display Generated where
  display (Generated n) = "g" ++ show n

generated :: Int -> Generated
generated = coerce

newtype Name = Name T.Text
  deriving (Eq, Ord, Show)

name :: T.Text -> Name
name = coerce

toLabel :: Name -> Label
toLabel = coerce

instance Display Name where
  display (Name s) = T.unpack s

newtype Record a = Record { getRecord :: Map.Map Label a }
  deriving (Eq, Show)
  deriving Functor
  deriving Foldable
  deriving (Semigroup, Monoid) -- Left-biased

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

instance Ftv.Ftv VProxy a => Ftv.Ftv VProxy (Record a) where
  ftv p (Record m) = foldMap (Ftv.ftv p) m

record :: [(Label, a)] -> Record a
record = Record . Map.fromList

intersection :: Record a -> Record b -> Set.Set Label
intersection (Record m1) (Record m2) = Map.keysSet m1 `Set.intersection` Map.keysSet m2

instance IsList (Record a) where
  type Item (Record a) = (Label, a)

  fromList = record
  toList (Record m) = Map.toList m

labels :: Record a -> [Label]
labels (Record m) = Map.keys m

foldMapIntersection :: (Applicative f, Monoid m) => (a -> a -> f m) -> Record a -> Record a -> f m
foldMapIntersection f (Record m1) (Record m2) = fmap fold $ sequenceA $ Map.intersectionWith f m1 m2

iter :: Monad m => (Label -> a -> m b) -> Record a -> m (Record b)
iter f (Record m) = coerce <$> Map.traverseWithKey f m

projRecord :: Label -> Record a -> Maybe a
projRecord l (Record m) = Map.lookup l m

updateRecord :: Label -> (a -> a) -> Record a -> Record a
updateRecord l f (Record m) = Record $ Map.adjust f l m

data Kind
  = Base
  | KFun Kind Kind
  deriving (Eq, Show)
  deriving Shift via Fixed Kind

instance Display Kind where
  displaysPrec _ Base         = showString "*"
  displaysPrec n (KFun k1 k2) = showParen (4 <= n) $ displaysPrec 4 k1 . showString " -> " . displaysPrec 3 k2

data VProxy

type instance Ftv.Variable VProxy = Variable

data BaseType
  = Bool
  | Int
  | Char
  deriving (Eq, Show)
  deriving Shift via Fixed BaseType
  deriving (Ftv.Ftv VProxy) via Ftv.Empty BaseType

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
  display = display . WithName

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

forall :: [Kind] -> Type -> Type
forall ks ty = foldl (flip Forall) ty ks

tabs :: [Kind] -> Type -> Type
tabs ks ty = foldl (flip TAbs) ty ks

tRecord :: Map.Map Label Type -> Type
tRecord = TRecord . Record

splitExQuantifiers :: Member (Error Failure) r => Int -> Type -> Eff r ([Kind], Type)
splitExQuantifiers 0 ty          = return ([], ty)
splitExQuantifiers n (Some k ty) = do
  (ks, ty) <- splitExQuantifiers (n - 1) ty
  return (k : ks, ty)
splitExQuantifiers _ ty = throw $ NotSome ty

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
  | If Term Term Term
  -- To make debug easy.
  | Let [Term] Term
  deriving (Eq, Show)

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
    let sep = if m == 0 then id else showString ", " in
    case mg of
      Nothing ->
        let f = displaysWithName 0 t1 in
        let ?nctx = newValue in
        let ?nctx = newTypes m in
          showParen (4 <= n) $ showString "unpack [" . displayVariable 0 . sep . displayTypeVariables m . showString "] = " . f . showString " in " . displaysWithName 0 t2
      Just g ->
        let f = displaysWithName 0 t1 in
        let ?nctx = newTypes m in
          showParen (4 <= n) $ showString "unpack [" . displays g . sep . displayTypeVariables m . showString "] = " . f . showString " in " . displaysWithName 0 t2
  displaysWithName n (Let ts t) =
    let fs = displaySemiWithName ts in
    let ?nctx = newValues $ length ts in
    let f = displaysWithName 0 t in
      showParen (4 <= n) $ showString "let " . appEndo (mconcat $ coerce $ intersperse (showString "; ") $ fst $ foldr (\f (fs, i) -> (displayVariable i . showString " = " . f : fs, i + 1)) ([], 0) fs) . showString " in " . f
  displaysWithName n (If t1 t2 t3) =
    showParen (4 <= n) $ showString "if " . displaysWithName 0 t1 . showString " then " . displaysWithName 0 t2 . showString " else " . displaysWithName 0 t3

displayTypeVariables :: (?nctx :: NameContext) => Int -> ShowS
displayTypeVariables 0 = id
displayTypeVariables 1 = displayTypeVariable 0
displayTypeVariables n = displayTypeVariable (n - 1) . showString ".." . displayTypeVariable 0

displaySemiWithName :: (DisplayName a, ?nctx :: NameContext) => [a] -> [ShowS]
displaySemiWithName = map $ displaysWithName 0

displayTypesRevWithName :: (?nctx :: NameContext) => [Type] -> ShowS
displayTypesRevWithName = appEndo . getDual . mconcat . coerce . intersperse (showString ", ") . map (displaysWithName 0)

var :: Int -> Term
var = Var . variable

poly :: [Kind] -> Term -> Term
poly ks t = foldl (flip Poly) t ks

inst :: Term -> [Type] -> Term
inst t tys = foldr (flip Inst) t tys

pack :: Term -> [Type] -> [Kind] -> Type -> Term
pack t [] []   _ = t
pack t tys ks ty
  | length tys /= length ks = error "pack: ill-formed expression"
  | otherwise               = Pack t tys ks ty

unpack :: Maybe Generated -> Term -> Int -> Term -> Term
unpack Nothing t1 0 = Let [t1]
unpack mg t1 n      = Unpack mg t1 n

data Env f ty = Env
  { tenv :: [f Kind]
  , venv :: [ty]
  , nmap :: Map.Map Name Int
  , tempVenv :: Map.Map Int ty
  }

emptyEnv :: Env f ty
emptyEnv = Env
  { tenv = []
  , venv = []
  , nmap = mempty
  , tempVenv = mempty
  }

tenvLen :: (?env :: Env f ty) => Int
tenvLen = length $ tenv ?env

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

insertTypes :: (Shift ty, ?env :: Env f ty) => [f Kind] -> Env f ty
insertTypes []       = ?env
insertTypes (k : ks) = let ?env = insertType k in insertTypes ks

insertValue :: (?env :: Env f ty) => Name -> ty -> Env f ty
insertValue name ty = ?env
  { venv = ty : venv ?env
  , nmap = Map.insert name (length (venv ?env) + 1) $ nmap ?env
  }

insertValueWithoutName :: (?env :: Env f ty) => ty -> Env f ty
insertValueWithoutName ty = ?env
  { venv = ty : venv ?env
  }

insertValuesWithoutName :: (?env :: Env f ty) => [ty] -> Env f ty
insertValuesWithoutName tys = ?env
  { venv = foldl (\xs ty -> ty : xs) (venv ?env) tys
  }

insertTempValue :: (?env :: Env f ty) => Generated -> ty -> Env f ty
insertTempValue (Generated n) ty = ?env
  { tempVenv = Map.insert n ty $ tempVenv ?env
  }

lookupType :: (Member (Error Failure) r, ?env :: Env f ty) => Variable -> Eff r (f Kind)
lookupType (Variable n) = do
  case tenv ?env of
    xs | 0 <= n && n < length xs -> return $ xs !! n
    _                            -> throw $ UnboundTypeVariable $ Variable n

lookupValueByName :: (Member (Error Failure) r, ?env :: Env f ty) => Name -> Eff r (ty, Variable)
lookupValueByName name = do
  n <- maybe (throw $ UnboundName name) return $ Map.lookup name $ nmap ?env
  let v = Variable $ length (venv ?env) - n
  ty <- lookupValue v
  return (ty, v)

lookupValue :: (Member (Error Failure) r, ?env :: Env f ty) => Variable -> Eff r ty
lookupValue (Variable n) = do
  case venv ?env of
    xs | 0 <= n && n < length xs -> return $ xs !! n
    _                            -> throw $ UnboundVariable $ Variable n

lookupTempValue :: (Member (Error Failure) r, ?env :: Env f ty) => Generated -> Eff r ty
lookupTempValue (Generated n) = do
  case Map.lookup n $ tempVenv ?env of
    Just ty -> return ty
    Nothing -> throw $ UnboundGeneratedVariable $ Generated n

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

-- Assumes input types have the base kind.
equalType :: Type -> Type -> Either Failure ()
equalType ty1 ty2 = run $ runError $ let ?env = emptyEnv :: Env Id Type in equal ty1 ty2 Base

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
    Base -> error $ "unexpected base kind, which is kind of " ++ display (WithName ty11)
strEquiv ty1 ty2 = throw $ StructurallyInequivalent ty1 ty2

reduce :: Type -> Type
reduce (TApp ty1 ty2) = reduce' (reduce ty1) ty2
reduce ty             = ty

reduce' :: Type -> Type -> Type
reduce' (TAbs _ ty1) ty2 = reduce $ substTop ty2 ty1
reduce' ty1 ty2          = TApp ty1 ty2

-- Polymorphic substitution.
newtype SubstP a = Subst (Map.Map Variable a)
  deriving (Eq, Show)
  deriving Functor

type Subst = SubstP Type

instance IsList (SubstP a) where
  type Item (SubstP a) = (Variable, a)

  fromList = coerce . Map.fromList
  toList (Subst m) = Map.toList m

instance Shift a => Shift (SubstP a) where
  shiftAbove c d (Subst m) = Subst $ fromList $ f <$> toList m
    where
      f (v, ty) = (shiftAbove c d v, shiftAbove c d ty)

lookupSubst :: Variable -> SubstP a -> Maybe a
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

newtype Id a = Id a

instance Annotated Id where
  extract (Id x) = x
  unannotated = Id

typecheck :: Term -> Either Failure Type
typecheck t = run $ runError $ let ?env = emptyEnv :: Env Id Type in whTypeOf t

whTypeOf :: (Typed a, Annotated f, Member (Error Failure) r, ?env :: Env f Type) => a -> Eff r Type
whTypeOf x = reduce <$> typeOf x

data TypeError
  = NotFunction Type
  | NotRecord Type
  | NotForall Type
  | NotSome Type
  | NotBool Type
  | TypeMismatch Type Type
  | MissingLabel Label (Record Type)
  | IllFormedPack [Type] [Kind]
  deriving (Eq, Show)

instance Display TypeError where
  display (NotFunction ty)       = "not function type: " ++ display ty
  display (NotRecord ty)         = "not record type: " ++ display ty
  display (NotForall ty)         = "not universal type: " ++ display ty
  display (NotSome ty)           = "not existential type: " ++ display ty
  display (NotBool ty)           = "not bool type: " ++ display ty
  display (TypeMismatch ty1 ty2) = "type mismatch: " ++ display ty1 ++ " and " ++ display ty2
  display (MissingLabel l r)     = "missing label (" ++ display l ++ ") in " ++ display r
  display (IllFormedPack tys ks) = "ill-formed 'pack': the number of witness types (" ++ show (length tys) ++ ") and that of existential quantifiers (" ++ show (length ks) ++ ")"

instance SpecificError TypeError where
  evidence = EvidType

typeOfLiteral :: Literal -> BaseType
typeOfLiteral (LBool _) = Bool
typeOfLiteral (LInt _)  = Int
typeOfLiteral (LChar _) = Char

instance Typed Literal where
  typeOf = return . BaseType . typeOfLiteral

instance Typed a => Typed (Record a) where
  typeOf (Record m) = tRecord <$> mapM typeOf m

lookupRecord :: Member (Error Failure) r => Label -> Record Type -> Eff r Type
lookupRecord l (Record m) = maybe (throw $ MissingLabel l $ Record m) return $ Map.lookup l m

-- Invariant: `typeOf` always returns, if any, a type which has the base kind.
instance Typed Term where
  typeOf (Lit l)     = typeOf l
  typeOf (Var v)     = lookupValue v
  typeOf (GVar g)    = lookupTempValue g
  typeOf (Abs ty t)  = mustBeBase ty >> let ?env = insertValueWithoutName ty in (TFun ty) <$> typeOf t
  typeOf (App t1 t2) = do
    ty1 <- whTypeOf t1
    ty2 <- typeOf t2
    case ty1 of
      TFun ty11 ty12 -> equal ty11 ty2 Base $> ty12
      _              -> throw $ NotFunction ty1
  typeOf (TmRecord r) = typeOf r
  typeOf (Proj t l)   = do
    ty <- whTypeOf t
    case ty of
      TRecord r -> lookupRecord l r
      _         -> throw $ NotRecord ty
  typeOf (Poly k t) = do
    let ?env = insertType $ unannotated k
    Forall k <$> typeOf t
  typeOf (Inst t ty) = do
    ty0 <- whTypeOf t
    k <- kindOf ty
    case ty0 of
      Forall k0 ty0
        | k0 == k   -> return $ substTop ty ty0
        | otherwise -> throw $ KindMismatch_ k0 k
      _ -> throw $ NotForall ty0
  typeOf (Pack t tys ks1 ty1) = do
    unless (length tys == length ks1) $
      throw $ IllFormedPack tys ks1
    ks2 <- mapM kindOf tys
    mapM_ (uncurry equalKind) $ zip ks1 ks2
    ty2 <- typeOf t
    let ty = some ks1 ty1
    mustBeBase ty
    equal (applyShiftNeg tys ty1) ty2 Base
    return ty
  typeOf (Unpack mg t1 n t2) = do
    ty <- whTypeOf t1
    (ks, ty) <- splitExQuantifiers n ty
    ty <-
      let ?env = insertTypes $ map unannotated ks in
      let ?env =
            case mg of
              Nothing -> insertValueWithoutName ty
              Just g  -> insertTempValue g ty
      in
      shift (-n) <$> typeOf t2
    mustBeBase ty
    return ty
  typeOf (Let ts t) = do
    tys <- mapM typeOf ts
    let ?env = insertValuesWithoutName tys
    typeOf t
  typeOf (If t1 t2 t3) = do
    ty1 <- whTypeOf t1
    case ty1 of
      BaseType Bool -> do
        ty2 <- typeOf t2
        ty3 <- typeOf t3
        equal ty2 ty3 Base
        return ty2
      _ -> throw $ NotBool ty1

equalKind :: Member (Error Failure) r => Kind -> Kind -> Eff r ()
equalKind k1 k2
  | k1 == k2  = return ()
  | otherwise = throw $ KindMismatch_ k1 k2

applyShiftNeg :: [Type] -> Type -> Type
applyShiftNeg tys ty =
  let n = length tys in
  let s = Subst $ Map.fromList $ zip (map Variable [0..]) (map (shift n) tys) in
    shift (-n) $ apply s ty
