{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Modules.Ros2018
  (
  -- * Objects
    Ident
  , ident

  -- * Syntax
  , Type(..)
  , Decl(..)
  , Expr(..)
  , Binding(..)

  -- * Useful functions
  , arrowP
  , arrowI

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
  , SemanticType(..)
  , AbstractType
  , Fun(..)

  -- * Paths
  , Path(..)
  , fromVariable

  -- * Parameterized semantic types
  , Parameterized(..)
  , parameterized

  -- * Subtyping
  , match'

  -- * Instantiation
  , lookupInsts

  -- * Embedding to internal objects
  , ToType(..)

  -- * Quantification
  , Existential
  , Universal
  , Quantification(..)

  -- * Substitution with small types
  , SubstitutionSmall(..)
  ) where

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Error
import Control.Monad.Freer.Fresh
import Data.Bifunctor
import Data.Coerce
import Data.Foldable
import Data.Functor
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Monoid hiding (First)
import Data.Proxy
import Data.Semigroup (First(..))
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Exts
import GHC.Generics

import Language.Modules.Ros2018.Display
import qualified Language.Modules.Ros2018.Ftv as Ftv
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal hiding (Env, Term(..), Type(..), Kind(..), TypeError(..), tabs)
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

data Decl
  = Spec Ident (Positional Type)
  | DInclude (Positional Type)
  deriving (Eq, Show)

instance Display Decl where
  displaysPrec _ (Spec id ty)  = displays id . showString " : " . displays ty
  displaysPrec _ (DInclude ty) = showString "include " . displaysPrec 5 ty

data Type
  = Base BaseType
  | TypeType
  | Arrow (Maybe Ident) (Positional Type) Purity (Positional Type)
  | Expr Expr
  | Singleton (Positional Expr)
  | Sig [Positional Decl]
  | Where (Positional Type) [Ident] (Positional Type)
  | WrapType (Positional Type)
  deriving (Eq, Show)

instance Display Type where
  displaysPrec _ (Base b)              = displays b
  displaysPrec _ TypeType              = showString "type"
  displaysPrec n (Arrow mid ty1 p ty2) =
    let arr = displayArrow p
        dom ty1 =
          case mid of
            Nothing -> displaysPrec 4 ty1
            Just id -> showParen True $ displays id . showString " : " . displays ty1
    in
    showParen (4 <= n) $ dom ty1 . showChar ' ' . showString arr . showChar ' ' . displaysPrec 3 ty2
  displaysPrec n (Expr e)            = displaysPrec n e
  displaysPrec _ (Singleton e)       = showParen True $ showString "= " . displays e
  displaysPrec _ (Sig ds)            = showString "sig " . appEndo (mconcat $ coerce $ intersperse (showString "; ") $ map (displays . fromPositional) ds) . showSpace (not $ null ds) . showString "end"
  displaysPrec n (Where ty1 ids ty2) =
    let f = appEndo $ mconcat $ coerce $ intersperse (showChar '.') $ map displays ids in
    showParen (4 <= n) $ displaysPrec 4 ty1 . showString " where (" . f . showString " : " . displays ty2 . showChar ')'
  displaysPrec n (WrapType ty) = showParen (4 <= n) $ showString "wrap " . displaysPrec 4 ty

showSpace :: Bool -> ShowS
showSpace True  = showChar ' '
showSpace False = id

displayArrow :: Purity -> String
displayArrow Pure   = "->"
displayArrow Impure = "~>"

arrowP :: Maybe Ident -> Positional Type -> Positional Type -> Type
arrowP mid ty1 ty2 = Arrow mid ty1 Pure ty2

arrowI :: Maybe Ident -> Positional Type -> Positional Type -> Type
arrowI mid ty1 ty2 = Arrow mid ty1 Impure ty2

data Binding
  = Val Ident (Positional Expr)
  | Include (Positional Expr)
  deriving (Eq, Show)

instance Display Binding where
  displaysPrec _ (Val id e)  = displays id . showString " = " . displays e
  displaysPrec _ (Include e) = showString "include " . displaysPrec 5 e

data Expr
  = Lit Literal
  | Id Ident
  | Struct [Positional Binding]
  | Type (Positional Type)
  | Seal (Positional Ident) (Positional Type)
  | Abs (Positional Ident) (Positional Type) (Positional Expr)
  | App (Positional Ident) (Positional Ident)
  | Proj (Positional Expr) Ident
  | If (Positional Ident) (Positional Expr) (Positional Expr) (Positional Type)
  | Wrap (Positional Ident) (Positional Type)
  | Unwrap (Positional Ident) (Positional Type)
  | Let [Positional Binding] (Positional Expr)
  deriving (Eq, Show)

instance Display Expr where
  displaysPrec _ (Lit l)          = displays l
  displaysPrec _ (Id id)          = displays id
  displaysPrec _ (Struct bs)      = showString "struct " . displaysBindings bs . showString "end"
  displaysPrec n (Type ty)        = showParen (4 <= n) $ showString "type " . displaysPrec 5 ty
  displaysPrec n (Seal id ty)     = showParen (4 <= n) $ displays id . showString " :> " . displaysPrec 4 ty
  displaysPrec n (Abs id ty e)    = showParen (4 <= n) $ showString "fun (" . displays id . showString " : " . displaysPrec 0 ty . showString ") => " . displaysPrec 3 e
  displaysPrec n (App id1 id2)    = showParen (4 <= n) $ displays id1 . showChar ' ' . displays id2
  displaysPrec _ (Proj e id)      = displaysPrec 4 e . showChar '.' . displays id
  displaysPrec n (If id e1 e2 ty) = showParen (4 <= n) $ showString "if " . displays id . showString " then " . displays e1 . showString " else " . displays e2 . showString " end : " . displays ty
  displaysPrec n (Wrap id ty)     = showParen (4 <= n) $ showString "wrap " . displays id  . showString " : " . displaysPrec 4 ty
  displaysPrec n (Unwrap id ty)   = showParen (4 <= n) $ showString "unwrap " . displays id  . showString " : " . displaysPrec 4 ty
  displaysPrec n (Let bs e)       = showParen (4 <= n) $ showString "let " . displaysBindings bs . showString "in " . displaysPrec 4 e

displaysBindings :: [Positional Binding] -> ShowS
displaysBindings bs = appEndo (mconcat $ coerce $ intersperse (showString "; ") $ map (displays . fromPositional) bs) . showSpace (not $ null bs)

instance Annotated Positional where
  extract (Positional _ x) = x
  unannotated = positional dummyPos

type Env = I.Env Positional SemanticType

data ElaborateError
  = NotStructure SemanticType
  | NotSubtype SemanticType SemanticType
  | NotSubpurity
  | PathMismatch Path Path
  | MissingLabel I.Label
  | NotPure
  | ImpureType Expr
  | NotReifiedType Position SemanticType Expr
  | NotEmptyExistential AbstractType
  | NotFunction Position SemanticType
  | NotBool Position SemanticType
  | NotWrappedType Position SemanticType
  | MissingExplicitType Position Expr
  | DuplicateSpec Position (Set.Set I.Label)
  | NoRealization SemanticType AbstractType Variable
  deriving (Eq, Show)

instance Display ElaborateError where
  display (NotStructure ty)         = "not structure type: " ++ display (WithName ty)
  display (NotSubtype ty1 ty2)      = display (WithName ty1) ++ " is not subtype of " ++ display (WithName ty2)
  display NotSubpurity              = "impure is not subtype of pure"
  display (PathMismatch p1 p2)      = "path mismatch: " ++ display (WithName p1) ++ " and " ++ display (WithName p2)
  display (MissingLabel l)          = "missing label: " ++ display l
  display NotPure                   = "not pure"
  display (ImpureType e)            = "unexpected impure type: " ++ display e
  display (NotReifiedType p ty e)   = display p ++ ": not reified type: " ++ display (WithName ty) ++ ", which is type of " ++ display e
  display (NotEmptyExistential aty) = "existentially quantified: " ++ display (WithName aty)
  display (NotFunction p ty)        = display p ++ ": not function type: " ++ display (WithName ty)
  display (NotBool p ty)            = display p ++ ": not bool type: " ++ display (WithName ty)
  display (NotWrappedType p ty)     = display p ++ ": not wrapped type: " ++ display (WithName ty)
  display (MissingExplicitType p e) = display p ++ ": expression without explicit type: " ++ display e
  display (DuplicateSpec p s)       = display p ++ ": this declaration duplicates specification(s): " ++ displaySet s
  display (NoRealization ty aty v)  = "could not find realization for " ++ display v ++ ": " ++ display (WithName ty) ++ " against " ++ display (WithName aty)

displaySet :: Display a => Set.Set a -> String
displaySet s = f ""
  where f = appEndo $ mconcat $ coerce $ intersperse (showString ", ") $ map displays $ Set.toList s

data Path = Path Variable [SemanticType]
  deriving (Eq, Show)
  deriving Generic

instance Shift Path

instance DisplayName Path where
  displaysWithName n p = displaysWithName n $ toType p

instance ToType Path where
  toType (Path v tys) = foldl I.TApp (I.TVar v) $ map toType tys

instance Ftv.Ftv VProxy Path where
  ftv p (Path v tys) = Set.insert v $ foldMap (Ftv.ftv p) $ tys

fromVariable :: Variable -> Path
fromVariable v = Path v []

equalPath :: (Member (Error ElaborateError) r, ?env :: Env) => Path -> Path -> Eff r ()
equalPath p1 @ (Path v1 tys1) p2 @ (Path v2 tys2)
  | v1 /= v2                   = throwError $ PathMismatch p1 p2
  | length tys1 /= length tys2 = throwError $ PathMismatch p1 p2
  | otherwise =
    -- TODO: It is perhaps wrong to assume the base kind.
    let f (ty1, ty2) = run $ runError $ equal (toType ty1) (toType ty2) I.Base in
    case mapM_ f $ zip tys1 tys2 of
      Right ()             -> return ()
      Left (Failure _ _ _) -> throwError $ PathMismatch p1 p2

data Fun = Fun SemanticType Purity AbstractType
  deriving (Eq, Show)
  deriving Generic

instance Shift Fun

instance DisplayName Fun where
  displaysWithName n (Fun ty p aty) =
    showParen (4 <= n) $ displaysWithName 4 ty . showString (" " ++ displayArrow p ++ " ") . displaysWithName 3 aty

instance Ftv.Ftv VProxy Fun

isPure :: Fun -> Bool
isPure (Fun _ p _) = p == Pure

codomain :: Fun -> AbstractType
codomain (Fun _ _ aty) = aty

domain :: Fun -> SemanticType
domain (Fun ty _ _) = ty

getPurity :: Fun -> Purity
getPurity (Fun _ p _) = p

data SemanticType
  = BaseType BaseType
  | Structure (Record SemanticType)
  | AbstractType AbstractType
  | SemanticPath Path
  | Function (Universal Fun)
  | Wrapped AbstractType
  deriving (Eq, Show)
  deriving Generic

instance Shift SemanticType

instance Ftv.Ftv VProxy SemanticType

instance DisplayName SemanticType where
  displaysWithName _ (BaseType b)       = displays b
  displaysWithName _ (Structure r)      = displaysWithName 0 r
  displaysWithName _ (AbstractType aty) = showString "[= " . displaysWithName 0 aty . showString "]"
  displaysWithName n (SemanticPath p)   = displaysWithName n p
  displaysWithName n (Function u)       =
    let ?nctx = newTypes $ qsLen u in
    let f = mconcat $ coerce $ intersperse (showString ", ") $ map (\(i, k) -> displayTypeVariable i . showString " : " . displays k) $ zip [0..] $ getKinds u in
    showParen (4 <= n) $ (\x -> showChar '∀' . appEndo f . showString ". " . displaysWithName 0 x) $ getBody u
  displaysWithName _ (Wrapped aty) = showString "[" . displaysWithName 0 aty . showString "]"

class SubstitutionSmall a where
  applySmall :: SubstP Parameterized -> a -> a

instance SubstitutionSmall SemanticType where
  applySmall _ ty @ (BaseType _)  = ty
  applySmall s (Structure r)      = Structure $ applySmall s <$> r
  applySmall s (AbstractType aty) = AbstractType $ applySmall s aty
  applySmall s (SemanticPath p)   = applyPath s p
  applySmall s (Function u)       = Function $ applySmall s u
  applySmall s (Wrapped aty)      = Wrapped $ applySmall s aty

instance SubstitutionSmall a => SubstitutionSmall (Existential a) where
  applySmall s e =
    let s1 = shift (qsLen e) s in
    qmap (applySmall s1) e

instance SubstitutionSmall a => SubstitutionSmall (Universal a) where
  applySmall s u =
    let s1 = shift (qsLen u) s in
    qmap (applySmall s1) u

instance SubstitutionSmall Fun where
  applySmall s (Fun ty p aty) = Fun (applySmall s ty) p (applySmall s aty)

instance SubstitutionSmall a => SubstitutionSmall [a] where
  applySmall s xs = applySmall s <$> xs

applyPath :: SubstP Parameterized -> Path -> SemanticType
applyPath s (Path v tys) =
  case lookupSubst v s of
    Nothing -> SemanticPath $ Path v $ applySmall s tys
    Just (Parameterized ks ty)
      | length ks == length tys -> applySmall (fromList $ zip (map variable [0..length ks-1]) $ map parameterized $ reverse tys) ty
      | otherwise               -> error "ill-formed semantic path"

newtype PathFormationError = PathFromType IType
  deriving (Eq, Show)

instance Display PathFormationError where
  display (PathFromType ty) = "cannot create well-formed semantic path from: " ++ display ty

appendPath :: [SemanticType] -> Path -> Path
appendPath tys' (Path v tys) = Path v $ tys ++ tys'

getStructure :: Member (Error ElaborateError) r => SemanticType -> Eff r (Record SemanticType)
getStructure (Structure r) = return r
getStructure ty            = throwError $ NotStructure ty

class Subtype a where
  type Coercion a

  (<:) :: (Member (Error ElaborateError) r, ?env :: Env) => a -> a -> Eff r (Coercion a)

instance Subtype Purity where
  type Coercion Purity = ()

  _ <: Impure    = return ()
  Pure <: Pure   = return ()
  Impure <: Pure = throwError NotSubpurity

instance Subtype SemanticType where
  type Coercion SemanticType = Term

  BaseType b1 <: BaseType b2
    | b1 == b2  = return $ I.Abs (I.BaseType b1) $ var 0
    | otherwise = throwError $ NotSubtype (BaseType b1) (BaseType b2)
  SemanticPath p1 <: SemanticPath p2     = equalPath p1 p2 $> I.Abs (toType p1) (var 0)
  AbstractType aty1 <: AbstractType aty2 = aty1 <: aty2 $> I.Abs (toType $ AbstractType aty1) (toTerm aty2)
  Structure r1 <: Structure r2           = I.Abs (toType r1) . I.TmRecord <$> I.iter f r2
    where
      f :: Member (Error ElaborateError) r => I.Label -> SemanticType -> Eff r Term
      f l ty2 = do
        ty1 <- maybe (throwError $ MissingLabel l) return $ projRecord l r1
        t <- ty1 <: ty2
        return $ I.App t $ var 0 `I.Proj` l
  Function u1 <: Function u2 = do
    let (Fun ty1 p1 aty1) = getBody u1
    let (Fun ty2 p2 aty2) = getBody u2
    let ?env = I.insertTypes $ reverse $ getAnnotatedKinds u2
    p1 <: p2
    (t1, tys) <- ty2 `match'` quantify (getAnnotatedKinds u1) ty1
    t2 <- aty1 <: aty2
    return $ I.Abs (toType u1) $ I.poly (getKinds u2) $ I.Abs (toType ty2) $ I.App t2 $ I.App (I.inst (var 1) tys) $ I.App t1 $ var 0
  Wrapped aty1 <: Wrapped aty2 =
    case run $ runError $ equal (toType aty1) (toType aty2) I.Base of
      Right ()             -> return $ I.Abs (toType aty1) $ var 0
      Left (Failure _ _ _) -> throwError $ NotSubtype (Wrapped aty1) (Wrapped aty2)
  ty1 <: ty2 = throwError $ NotSubtype ty1 ty2

instance Subtype AbstractType where
  type Coercion AbstractType = Term

  aty1 <: aty2 = do
    let ?env = insertTypes $ reverse $ getAnnotatedKinds aty1
    (t, tys) <- match' (getBody aty1) aty2
    return $ I.Abs (toType aty1) $ I.unpack Nothing (var 0) (qsLen aty1) $ I.pack (I.App t $ var 0) tys (getKinds aty2) $ toType (getBody aty2)

match :: (Member (Error ElaborateError) r, ?env :: Env) => SemanticType -> AbstractType -> Eff r (Term, [Parameterized])
match ty aty = do
  tys <- either (throwError . NoRealization ty aty) return $ lookupInsts (enumVars aty) ty (getBody aty)
  t <- ty <: shift (-qsLen aty) (applySmall (fromList $ zip (enumVars aty) $ shift (qsLen aty) tys) $ getBody aty)
  return (t, tys)

match' :: (Member (Error ElaborateError) r, ?env :: Env) => SemanticType -> AbstractType -> Eff r (Term, [IType])
match' ty aty = do
  (t, tys) <- match ty aty
  return (t, toType <$> tys)

class Sized a where
  isSmall :: a -> Bool

instance Sized SemanticType where
  isSmall (BaseType _)       = True
  isSmall (Structure r)      = all isSmall r
  isSmall (AbstractType aty) = isSmall aty
  isSmall (SemanticPath _)   = True -- maybe
  isSmall (Function u)       = isSmall u
  isSmall (Wrapped _)        = True

instance Sized Fun where
  isSmall (Fun _ Pure _)      = False
  isSmall (Fun ty Impure aty) = isSmall ty && isSmall aty

newtype Quantified a = Quantified ([Positional IKind], a)
  deriving (Eq, Show)
  deriving Functor

instance Ftv.Ftv VProxy a => Ftv.Ftv VProxy (Quantified a) where
  ftv p (Quantified (ks, x)) = foldMap f $ Ftv.ftv p x
    where
      f :: Variable -> Set.Set Variable
      f v
        | v < variable (length ks) = mempty
        | otherwise                = Set.singleton $ v -: variable (length ks)

class Quantification f where
  getKinds :: f a -> [IKind]
  getAnnotatedKinds :: f a -> [Positional IKind]
  qsLen :: f a -> Int
  enumVars :: f a -> [Variable]
  getBody :: f a -> a
  fromBody :: a -> f a
  quantify :: [Positional IKind] -> a -> f a
  qmap :: (a -> b) -> f a -> f b

instance Quantification Quantified where
  getKinds (Quantified (ks, _)) = map fromPositional ks
  getAnnotatedKinds (Quantified (ks, _)) = ks
  qsLen (Quantified (ks, _)) = length ks
  enumVars q = map variable [0..qsLen q-1]
  getBody (Quantified (_, x)) = x
  fromBody x = Quantified ([], x)
  quantify ks x = Quantified (ks, x)
  qmap f q = quantify (getAnnotatedKinds q) $ f $ getBody q

qfmap :: (Quantification q, Functor f) => (a -> f b) -> q a -> f (q b)
qfmap f q = quantify (getAnnotatedKinds q) <$> f (getBody q)

instance Shift a => Shift (Quantified a) where
  shiftAbove c d q = qmap (shiftAbove (c + qsLen q) d) q

instance Sized a => Sized (Quantified a) where
  isSmall q
    | qsLen q == 0 = isSmall $ getBody q
    | otherwise    = False

newtype Existential a = Existential (Quantified a)
  deriving (Eq, Show)
  deriving Functor
  deriving Quantification
  deriving Shift
  deriving Sized

instance DisplayName a => DisplayName (Existential a) where
  displaysWithName _ (Existential (Quantified (ks, x))) =
    let ?nctx = newTypes $ length ks in
    let f = mconcat $ coerce $ intersperse (showString ", ") $ map (\(i, k) -> displayTypeVariable i . showString " : " . displays k) $ zip [0..] ks in
    showString "∃" . appEndo f . showString ". " . displaysWithName 0 x

deriving instance Ftv.Ftv VProxy a => Ftv.Ftv VProxy (Existential a)

newtype Universal a = Universal (Quantified a)
  deriving (Eq, Show)
  deriving Functor
  deriving Quantification
  deriving Shift
  deriving Sized

deriving instance Ftv.Ftv VProxy a => Ftv.Ftv VProxy (Universal a)

toUniversal :: Existential a -> Universal a
toUniversal = coerce

toExistential :: Universal a -> Existential a
toExistential = coerce

type AbstractType = Existential SemanticType

instance ToType AbstractType where
  toType (Existential (Quantified (ks, ty))) = I.some (map fromPositional ks) $ toType ty

instance ToType a => ToType (Universal a) where
  toType u = I.forall (getKinds u) $ toType $ getBody u

data Purity
  = Pure
  | Impure
  deriving (Eq, Show)
  deriving Shift via Fixed Purity
  deriving (Ftv.Ftv VProxy) via Ftv.Empty Purity

instance Display Purity where
  display Pure   = "pure"
  display Impure = "impure"

-- Join.
instance Semigroup Purity where
  Pure <> Pure = Pure
  _ <> _       = Impure

class ToType a where
  toType :: a -> IType

instance ToType Fun where
  toType (Fun ty _ aty) = toType ty `I.TFun` toType aty

instance ToType SemanticType where
  toType (BaseType b)       = I.BaseType b
  toType (Structure r)      = toType r
  toType (AbstractType aty) = toType aty `I.TFun` I.TRecord (record [])
  toType (SemanticPath p)   = toType p
  toType (Function f)       = toType f
  toType (Wrapped aty)      = toType aty

instance ToType Parameterized where
  toType (Parameterized ks ty) = I.tabs (reverse ks) $ toType ty

instance ToType a => ToType (Record a) where
  toType r = I.TRecord $ toType <$> r

toTerm :: AbstractType -> Term
toTerm aty = I.Abs (toType aty) $ I.TmRecord $ record []

data Parameterized = Parameterized [IKind] SemanticType
  deriving (Eq, Show)

instance Shift Parameterized where
  shiftAbove c d (Parameterized ks ty) = Parameterized ks $ shiftAbove (c + length ks) d ty

parameterized :: SemanticType -> Parameterized
parameterized = Parameterized []

tabs :: [IKind] -> Parameterized -> Parameterized
tabs ks1 (Parameterized ks ty) = Parameterized (reverse ks1 ++ ks) ty

pattern EmptyExistential :: SemanticType -> SemanticType
pattern EmptyExistential x <- AbstractType (Existential (Quantified ([], x)))

type LError = Either Variable

lookupInst :: Path -> SemanticType -> SemanticType -> LError (Maybe (First Parameterized))
lookupInst p1 (EmptyExistential ty) (EmptyExistential (SemanticPath p2))
  | p1 == p2 && isSmall ty = return $ Just $ First $ Parameterized [] ty
  | otherwise              = return $ Nothing
lookupInst p (Structure r1) (Structure r2) = I.foldMapIntersection (lookupInst p) r1 r2
lookupInst p (Function u1) (Function u2)
  | isPure (getBody u1) && isPure (getBody u2) = do
    ps <- lookupInsts (enumVars u1) (domain $ getBody u2) (domain $ getBody u1)
    let s = fromList $ zip (map variable [0..]) ps
    mfty <- lookupInst (appendPath (map (SemanticPath . fromVariable) $ enumVars u2) $ shift (qsLen u2) p) (applySmall s $ getBody $ codomain $ getBody u1) (getBody $ codomain $ getBody u2)
    return $ fmap (tabs $ getKinds u2) <$> mfty
lookupInst _ _ _ = return Nothing

lookupInsts :: [Variable] -> SemanticType -> SemanticType -> LError [Parameterized]
lookupInsts vs ty1 ty2 = fst <$> foldrM f ([], ty2) vs
  where
    f :: Variable -> ([Parameterized], SemanticType) -> LError ([Parameterized], SemanticType)
    f v (tys, ty) = (\r -> (r : tys, applySmall [(v, r)] ty)) <$> res
      where
        res :: LError Parameterized
        res = lookupInst (fromVariable v) ty1 ty >>= (coerce . maybe (Left v) Right)

translate :: Positional Expr -> Either I.Failure (Either ElaborateError (Term, AbstractType, Purity))
translate e = run $ runError $ runError $ evalFresh 0 $ let ?env = I.emptyEnv in elaborate e

runElaborate :: Eff '[Fresh, Error ElaborateError, Error I.Failure] a -> Either I.Failure (Either ElaborateError a)
runElaborate x = run $ runError $ runError $ evalFresh 0 x

class Elaboration a where
  type Output a
  type Effs a :: [* -> *]

  elaborate :: (Members (Effs a) r, ?env :: Env) => Positional a -> Eff r (Output a)

freshName :: Member Fresh r => Eff r Name
freshName = do
  n <- fresh
  return $ name $ "?d" <> T.pack (show n)

pattern EmptyExistential1 :: a -> Existential a
pattern EmptyExistential1 x <- Existential (Quantified ([], x))

instance Elaboration Type where
  type Output Type = AbstractType
  type Effs Type = '[Error I.Failure, Error ElaborateError, Fresh]

  elaborate (Positional _ (Base b)) = return $ fromBody $ BaseType b
  elaborate (Positional p TypeType) = return $ quantify [positional p I.Base] $ AbstractType $ fromBody $ SemanticPath $ fromVariable $ variable 0
  elaborate (Positional _ (Arrow mid ty1 p ty2)) = do
    name <- case mid of
      Nothing -> freshName
      Just id -> return $ coerce id
    aty1 <- elaborate ty1
    let ?env = I.insertTypes $ reverse $ getAnnotatedKinds aty1
    let ?env = insertValue name $ getBody aty1
    aty2 <- elaborate ty2
    case p of
      Impure -> do
        let f ty = Fun ty Impure aty2
        return $ fromBody $ Function $ qmap f $ toUniversal aty1
      Pure -> do
        let aty1' = shift (qsLen aty2) aty1
        let a = zip (enumVars aty2) $ map (\v -> parameterized $ SemanticPath $ Path v $ SemanticPath . fromVariable <$> enumVars aty1) $ shift (qsLen aty1) $ enumVars aty2
        let b = zip (shift (qsLen aty2) $ enumVars aty1) $ parameterized . SemanticPath . fromVariable <$> enumVars aty1
        let s = fromList $ a ++ b
        let aty2' = quantify (fmap (\k -> foldr I.KFun k $ getKinds aty1) <$> getAnnotatedKinds aty2) $ applySmall s $ getBody aty2
        return $ qmap (\ty2 -> Function $ qmap (\ty1 -> Fun ty1 Pure $ fromBody ty2) $ toUniversal aty1') aty2'
  elaborate (Positional p (Expr e)) = do
    z <- elaborate $ positional p e
    case z of
      (_, _, Impure)                               -> throwError $ ImpureType e
      (_, EmptyExistential1 (AbstractType aty), _) -> return aty
      (_, EmptyExistential1 ty, _)                 -> throwError $ NotReifiedType p ty e
      (_, aty, _)                                  -> throwError $ NotEmptyExistential aty
  elaborate (Positional _ (Singleton e)) = do
    withExplicitType e
    z <- elaborate e
    case z of
      (_, _, Impure)               -> throwError $ ImpureType $ fromPositional e
      (_, EmptyExistential1 ty, _) -> return $ fromBody ty
      (_, _, _)                    -> error "in the absence of weak sealing, a pure expression must not be given an existential type"
  elaborate (Positional _ (Sig ds)) = do
    (aty, _) <- foldlM elaborateDecls (fromBody [], ?env) ds
    return $ Structure <$> aty
  elaborate (Positional _ (Where ty1 ids ty2)) = do
    aty1 <- elaborate ty1
    aty2 <- elaborate ty2
    ty <- proj (getBody aty1) ids
    let vs = ftv ty
    let (vs12, vs11) = (`Set.member` vs) `Set.partition` (fromList $ enumVars aty1)
    let a = zip (Set.toAscList vs12) [0..]
    let b = zip (Set.toAscList vs11) [Set.size vs12..]
    let s = fromList $ map (second $ parameterized . SemanticPath . fromVariable . variable) $ a ++ b
    let ty' = shiftAbove (qsLen aty1) (qsLen aty2) $ applySmall s ty
    let ?env = I.insertTypes $ reverse $ getAnnotatedKinds aty2
    let n = tenvLen
    let z = restrict vs11 $ getAnnotatedKinds aty1
    let ?env = I.insertTypes $ reverse z
    (_, tys) <- getBody aty2 `match` quantify (restrict vs12 $ getAnnotatedKinds aty1) ty'
    let r = take n $ [ (variable i, parameterized $ SemanticPath $ fromVariable $ variable $ i - Set.size vs12) | i <- [qsLen aty1..] ]
    return $ quantify (z ++ getAnnotatedKinds aty2) $ replace (applySmall (fromList $ zip (Set.toAscList vs12) tys ++ zip (Set.toAscList vs11) (parameterized . SemanticPath . fromVariable . variable <$> [0..]) ++ r) $ shiftAbove (qsLen aty1) (qsLen aty2) $ getBody aty1) ids $ getBody aty2
  elaborate (Positional _ (WrapType ty)) = fromBody . Wrapped <$> elaborate ty

-- Assumes @proj ty1 ids@ succeeded before @replace ty1 ids ty2@.
replace :: SemanticType -> [Ident] -> SemanticType -> SemanticType
replace _ [] ty                     = ty
replace (Structure r) (id : ids) ty = Structure $ updateRecord (toLabel $ coerce id) (\x -> replace x ids ty) r
replace ty _ _ = error $ "not structure: " ++ display (WithName ty)

ftv :: SemanticType -> Set.Set Variable
ftv = Ftv.ftv (Proxy :: Proxy VProxy)

restrict :: (?env :: Env) => Set.Set Variable -> [Positional IKind] -> [Positional IKind]
restrict vs ks =
  let m = Map.fromList $ zip (map variable [0..]) ks in
  Map.elems $ Map.restrictKeys m vs

proj :: Member (Error ElaborateError) r => SemanticType -> [Ident] -> Eff r SemanticType
proj ty []                    = return ty
proj (Structure r) (id : ids) =
  let l = toLabel $ coerce id in
  case projRecord l r of
    Just ty -> proj ty ids
    Nothing -> throwError $ MissingLabel l
proj ty _ = throwError $ NotStructure ty

elaborateDecls :: Members '[Error I.Failure, Error ElaborateError, Fresh] r => (Existential (Record SemanticType), Env) -> Positional Decl -> Eff r (Existential (Record SemanticType), Env)
elaborateDecls (acc, env) d = do
  let ?env = env
  aty <- elaborate d
  let ?env = I.insertTypes $ reverse $ getAnnotatedKinds aty
  let ?env = foldl (\env (l, ty) -> let ?env = env in insertValue (toName l) ty) ?env $ I.toList $ getBody aty
  mustBeDisjoint (getPosition d) acc aty
  return (merge acc aty, ?env)

mustBeDisjoint :: Member (Error ElaborateError) r => Position -> Existential (Record SemanticType) -> Existential (Record SemanticType) -> Eff r ()
mustBeDisjoint p e1 e2 =
  let s = I.intersection (getBody e1) $ getBody e2 in
  if Set.null s
    then return ()
    else throwError $ DuplicateSpec p s

instance Elaboration Decl where
  type Output Decl = Existential (Record SemanticType)
  type Effs Decl = '[Error I.Failure, Error ElaborateError, Fresh]

  elaborate (Positional _ (Spec id ty))  = qmap (\ty -> [(toLabel $ coerce id, ty)]) <$> elaborate ty
  elaborate (Positional _ (DInclude ty)) = elaborate ty >>= qfmap getStructure

withExplicitType :: Member (Error ElaborateError) r => Positional Expr -> Eff r ()
withExplicitType (Positional p e) =
  case e of
    Seal _ _ -> return ()
    Type _   -> return ()
    _        -> throwError $ MissingExplicitType p e

instance Elaboration Literal where
  type Output Literal = BaseType
  type Effs Literal = '[]

  elaborate = return . I.typeOfLiteral . fromPositional

mustBePure :: Member (Error ElaborateError) r => Purity -> Eff r ()
mustBePure Pure   = return ()
mustBePure Impure = throwError NotPure

strongSealing :: AbstractType -> Purity
strongSealing aty
  | qsLen aty == 0 = Pure
  | otherwise      = Impure

instance Elaboration Expr where
  type Output Expr = (Term, AbstractType, Purity)
  type Effs Expr = '[Error I.Failure, Error ElaborateError, Fresh]

  elaborate (Positional pos (Lit l)) = do
    b <- elaborate $ Positional pos l
    return (I.Lit l, fromBody $ BaseType b, Pure) -- Literals are always pure.
  elaborate (Positional _ (Id id)) = do
    (ty, v) <- lookupValueByName $ coerce id
    return (I.Var v, fromBody ty, Pure)
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
  elaborate (Positional _ (Seal id ty)) = do
    (t1, aty1, p) <- elaborate $ Id <$> id
    mustBePure p
    aty2 <- elaborate ty
    (t2, tys) <- match' (getBody aty1) aty2
    return (I.pack (I.App t2 t1) tys (getKinds aty2) (toType $ getBody aty2), aty2, strongSealing aty2)
  elaborate (Positional _ (Abs id ty e)) = do
    aty1 <- elaborate ty
    let ?env = insertTypes $ reverse $ getAnnotatedKinds aty1
    let ?env = insertValue (coerce $ fromPositional id) $ getBody aty1
    (t, aty2, p) <- elaborate e
    return (I.poly (getKinds aty1) $ I.Abs (toType $ getBody aty1) $ t, fromBody $ Function $ qmap (\ty -> Fun ty p aty2) $ toUniversal aty1, Pure)
  elaborate (Positional _ (App id1 id2)) = do
    (t1, aty1, _) <- elaborate $ Id <$> id1
    (t2, aty2, _) <- elaborate $ Id <$> id2
    case getBody aty1 of
      Function u -> do
        (t3, tys) <- match (getBody aty2) $ toExistential $ qmap domain u
        return (I.App (I.inst t1 $ toType <$> tys) $ I.App t3 t2, shift (-qsLen u) $ applySmall (fromList $ zip (enumVars u) $ shift (qsLen u) tys) $ codomain $ getBody u, getPurity $ getBody u)
      ty1 -> throwError $ NotFunction (getPosition id1) ty1
  elaborate (Positional _ (Proj e id)) = do
    (t, aty, p) <- elaborate e
    r <- getStructure $ getBody aty
    let l = toLabel $ coerce id
    ty <- maybe (throwError $ MissingLabel l) return $ projRecord l r
    let aty1 = qmap (const ty) aty
    return (I.unpack Nothing t (qsLen aty) $ I.pack (I.Proj (var 0) l) (I.TVar <$> enumVars aty) (getKinds aty) $ toType $ getBody aty1, aty1, p)
  elaborate (Positional _ (If id e2 e3 ty)) = do
    (t1, aty1, _) <- elaborate $ Id <$> id
    case getBody aty1 of
      BaseType I.Bool -> do
        aty <- elaborate ty
        (t2, aty2, p2) <- elaborate e2
        (t3, aty3, p3) <- elaborate e3
        f2 <- aty2 <: aty
        f3 <- aty3 <: aty
        return (I.If t1 (I.App f2 t2) $ I.App f3 t3, aty, p2 <> p3 <> strongSealing aty)
      ty -> throwError $ NotBool (getPosition id) ty
  elaborate (Positional _ (Wrap id ty)) = do
    (t1, aty1, _) <- elaborate $ Id <$> id
    aty2 <- elaborate ty
    case aty2 of
      EmptyExistential1 (Wrapped aty2') -> do
        t2 <- aty1 <: aty2'
        return (I.App t2 t1, aty2, Pure)
      EmptyExistential1 ty' -> throwError $ NotWrappedType (getPosition ty) ty'
      _ -> throwError $ NotEmptyExistential aty2
  elaborate (Positional _ (Unwrap id ty)) = do
    (t1, aty1, _) <- elaborate $ Id <$> id
    aty1' <- case getBody aty1 of
      Wrapped aty1' -> return aty1'
      ty            -> throwError $ NotWrappedType (getPosition id) ty
    aty2 <- elaborate ty
    case aty2 of
      EmptyExistential1 (Wrapped aty2') -> do
        t2 <- aty1' <: aty2'
        return (I.App t2 t1, aty2', strongSealing aty2')
      EmptyExistential1 ty' -> throwError $ NotWrappedType (getPosition ty) ty'
      _ -> throwError $ NotEmptyExistential aty2
  elaborate (Positional p (Let bs e)) = do
    id <- coerce <$> freshName
    -- TODO: Handle positions correctly.
    elaborate $ positional p $ Proj (positional p $ Struct $ bs ++ [positional p $ Val id e]) id

buildRecord :: [[I.Label]] -> Map.Map I.Label Term
buildRecord lls = fst $ foldl f (mempty, 0) lls
  where
    f = foldr (\l (m', n') -> (Map.insertWith (\_ x -> x) l (var n') m', n' + 1))

joinBindings :: Member Fresh r => Term -> (Term, Int, [I.Label]) -> Eff r Term
joinBindings acc (t, n, ls) = do
  g <- I.generated <$> fresh
  return $ I.unpack (Just g) t n $ I.Let (map (I.Proj $ I.GVar g) ls) acc

type Acc = (Env, Existential (Record SemanticType), [(Term, Int, [I.Label])], Purity)

elaborateBindings :: (Members (Effs Expr) r, ?env :: Env) => Acc -> Positional Binding -> Eff r Acc
elaborateBindings (env, whole_aty, zs, p0) b = do
  let ?env = env
  (t, aty, p) <- elaborate b
  let ?env = I.insertTypes $ reverse $ getAnnotatedKinds aty
  let r = getBody aty
  let ?env = foldl (\env (l, ty) -> let ?env = env in insertValue (toName l) ty) ?env $ I.toList r
  return (?env, merge whole_aty $ quantify (getAnnotatedKinds aty) r, (t, qsLen aty, I.labels r) : zs, p0 <> p)

merge :: Existential (Record SemanticType) -> Existential (Record SemanticType) -> Existential (Record SemanticType)
merge aty1 aty2 =
  let ty1 = shift (qsLen aty2) $ getBody aty1 in
  quantify (getAnnotatedKinds aty2 ++ getAnnotatedKinds aty1) $ getBody aty2 <> ty1

instance Elaboration Binding where
  type Output Binding = (Term, Existential (Record SemanticType), Purity)
  type Effs Binding = '[Error I.Failure, Error ElaborateError, Fresh]

  elaborate (Positional _ (Val id e)) = do
    (t, aty, p) <- elaborate e
    let l = I.toLabel $ coerce id
    return (I.unpack Nothing t (qsLen aty) $ I.pack (I.TmRecord $ record [(l, var 0)]) (I.TVar <$> enumVars aty) (getKinds aty) $ I.TRecord $ (\x -> [(l, x)]) $ toType $ getBody aty, (\x -> record [(l, x)]) <$> aty, p)

  elaborate (Positional _ (Include e)) = do
    (t, aty, p) <- elaborate e
    r <- getStructure $ getBody aty
    return (t, quantify (getAnnotatedKinds aty) r, p)
