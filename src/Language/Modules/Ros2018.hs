{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  , unIdent

  -- * Syntax
  , Type(..)
  , Decl(..)
  , Param(..)
  , Expr(..)
  , Binding(..)
  , Asc(..)

  -- * Useful functions
  , arrowP
  , arrowI
  , val

  -- * Environments
  , Env

  -- * Elaboration
  , Elaboration(..)

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
  , applyPath

  -- * Monads
  , FreshM(..)
  , PrimAM(..)
  , ErrorM(..)
  , RunFailureM(..)

  -- * Others
  , Z(..)
  ) where

import Data.Bifunctor
import Data.Coerce
import Data.Foldable
import Data.Functor
import Data.List
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
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
import Language.Modules.Ros2018.Internal hiding (Env, Term(..), Type(..), Kind(..), TypeError(..), tabs, Id(..))
import Language.Modules.Ros2018.Internal (Term, ToType(..))
import Language.Modules.Ros2018.Position
import Language.Modules.Ros2018.Shift

type IType = I.Type
type IKind = I.Kind

newtype Ident = Ident Name
  deriving (Eq, Ord, Show)

ident :: T.Text -> Ident
ident = Ident . name

unIdent :: Ident -> Name
unIdent = coerce

instance Display Ident where
  display (Ident name) = display name

data Decl
  = Spec Ident [Param] (Positional Type)
  | AbsTypeSpec Ident [Param]
  | ManTypeSpec Ident [Param] (Positional Type) -- Manifest type specification.
  | DInclude (Positional Type)
  deriving (Eq, Show)

instance Display Decl where
  displaysPrec _ (Spec id ps ty)        = displays id . showSpace (not $ null ps) . displaysParams ps . showString " : " . displays ty
  displaysPrec _ (AbsTypeSpec id ps)    = showString "type " . displays id . showSpace (not $ null ps) . displaysParams ps
  displaysPrec _ (ManTypeSpec id ps ty) = showString "type " . displays id . showSpace (not $ null ps) . displaysParams ps . showString " = " . displays ty
  displaysPrec _ (DInclude ty)          = showString "include " . displaysPrec 5 ty

spec :: Ident -> Positional Type -> Decl
spec id = Spec id []

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

data Asc
  = Trans
  | Opaque
  deriving (Eq, Show)

instance Display Asc where
  display Trans = ":"
  display Opaque = ":>"

data Binding
  = Val Ident [Param] (Maybe (Asc, Positional Type)) (Positional Expr)
  | TypeBinding Ident [Param] (Positional Type)
  | Include (Positional Expr)
  | Open (Positional Expr)
  | Local [Positional Binding] [Positional Binding]
  | Primitive Ident T.Text
  deriving (Eq, Show)

instance Display Binding where
  displaysPrec _ (Val id ps asc e)      = displays id . showSpace True . displaysParams ps . maybe (\s -> s) (\(x, ty) -> showSpace (not $ null ps) . displays x . showSpace True . displays ty) asc . showString "= " . displays e
  displaysPrec _ (TypeBinding id ps ty) = showString "type " . displays id . showSpace (not $ null ps) . displaysParams ps . showString " = " . displays ty
  displaysPrec _ (Include e)            = showString "include " . displaysPrec 5 e
  displaysPrec _ (Open e)               = showString "open " . displaysPrec 5 e
  displaysPrec n (Local bs1 bs2)        = showParen (4 <= n) $ showString "local " . displaysBindings bs1 . showString "in " . displaysBindings bs2 . showString "end"
  displaysPrec _ (Primitive id t)       = showString "primitive " . displays id . showString " = " . displays t

instance Display T.Text where
  display = show

val :: Ident -> Positional Expr -> Binding
val id = Val id [] Nothing

noReexport :: Binding -> Bool
noReexport (Open _) = True
noReexport _        = False

data Param
  = Param (Positional Ident) (Positional Type)
  | Omit (Positional Ident)
  deriving (Eq, Show)

instance Display Param where
  displaysPrec _ (Param id ty) = showParen True $ displays id . showString " : " . displays ty
  displaysPrec _ (Omit id)     = displays id

class Interpose f where
  interpose :: a -> f a -> f a

instance Interpose [] where
  interpose = intersperse

instance Interpose NonEmpty where
  interpose = NonEmpty.intersperse

displaysParams :: (Interpose f, Functor f, Foldable f, Display a) => f a -> ShowS
displaysParams params = appEndo $ fold $ fmap coerce $ interpose (showChar ' ') $ displays <$> params

data Expr
  = Lit Literal
  | Id Ident
  | Struct [Positional Binding]
  | Type (Positional Type)
  | Seal (Positional Expr) (Positional Type)
  | TransparentAsc (Positional Expr) (Positional Type)
  | Abs (NonEmpty Param) (Positional Expr)
  | App (Positional Expr) (Positional Expr)
  | Proj (Positional Expr) Ident
  | If (Positional Expr) (Positional Expr) (Positional Expr) (Positional Type)
  | Wrap (Positional Expr) (Positional Type)
  | Unwrap (Positional Expr) (Positional Type)
  | Let [Positional Binding] (Positional Expr)
  | LetOp (Positional Ident) (Positional Ident) (Positional Type) (Positional Expr) (Positional Expr)
  | OpenE (Positional Expr) (Positional Expr)
  | Fix
  deriving (Eq, Show)

instance Display Expr where
  displaysPrec _ (Lit l)                = displays l
  displaysPrec _ (Id id)                = displays id
  displaysPrec _ (Struct bs)            = showString "struct " . displaysBindings bs . showString "end"
  displaysPrec n (Type ty)              = showParen (4 <= n) $ showString "type " . displaysPrec 5 ty
  displaysPrec n (Seal e ty)            = showParen (4 <= n) $ displaysPrec 9 e . showString " :> " . displaysPrec 4 ty
  displaysPrec n (TransparentAsc e ty)  = showParen (4 <= n) $ displaysPrec 9 e . showString " : " . displaysPrec 4 ty
  displaysPrec n (Abs params e)         = showParen (4 <= n) $ showString "fun " . displaysParams params . showString " => " . displaysPrec 3 e
  displaysPrec n (App e1 e2)            = showParen (4 <= n) $ displaysPrec 4 e1 . showChar ' ' . displaysPrec 5 e2
  displaysPrec _ (Proj e id)            = displaysPrec 4 e . showChar '.' . displays id
  displaysPrec n (If e0 e1 e2 ty)       = showParen (4 <= n) $ showString "if " . displays e0 . showString " then " . displays e1 . showString " else " . displays e2 . showString " end : " . displays ty
  displaysPrec n (Wrap e ty)            = showParen (4 <= n) $ showString "wrap " . displaysPrec 4 e  . showString " : " . displaysPrec 4 ty
  displaysPrec n (Unwrap e ty)          = showParen (4 <= n) $ showString "unwrap " . displaysPrec 4 e  . showString " : " . displaysPrec 4 ty
  displaysPrec n (Let bs e)             = showParen (4 <= n) $ showString "let " . displaysBindings bs . showString "in " . displaysPrec 4 e
  displaysPrec n (LetOp op id ty e1 e2) = showParen (4 <= n) $ displays op . showString " " . displays id . showString " : " . displaysPrec 0 ty . showString " = " . displaysPrec 0 e1 . showString "in " . displaysPrec 4 e2
  displaysPrec n (OpenE e1 e2)          = showParen (4 <= n) $ showString "open " . displays e1 . showString "in " . displaysPrec 4 e2
  displaysPrec _ Fix                    = showString "fix"

displaysBindings :: [Positional Binding] -> ShowS
displaysBindings bs = appEndo (mconcat $ coerce $ intersperse (showString "; ") $ map (displays . fromPositional) bs) . showSpace (not $ null bs)

instance Annotated Positional where
  extract (Positional _ x) = x
  unannotated = positional dummyPos

type Env = I.Env Positional SemanticType

data ElaborateError
  = NotStructure Position SemanticType
  | NotSubtype SemanticType SemanticType
  | NotSubpurity
  | PathMismatch Path Path
  | MissingLabel T.Text I.Label [I.Label]
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
  | NotMatch SemanticType AbstractType ElaborateError
  deriving (Eq, Show)

instance Display ElaborateError where
  display (NotStructure p ty)       = display p ++ ": not structure type: " ++ display (WithName ty)
  display (NotSubtype ty1 ty2)      = display (WithName ty1) ++ " is not subtype of " ++ display (WithName ty2)
  display NotSubpurity              = "impure is not subtype of pure"
  display (PathMismatch p1 p2)      = "path mismatch: " ++ display (WithName p1) ++ " and " ++ display (WithName p2)
  display (MissingLabel s l ls)     = T.unpack s ++ ": missing label: " ++ display l ++ ": " ++ show ls
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
  display (NotMatch ty aty e)       = display (WithName ty) ++ " does not match against " ++ display (WithName aty) ++ ": " ++ display e

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

data Z a = forall k. FailureM k => Z (k a -> Either Failure a)

class Monad m => RunFailureM m where
  runF :: m (Z a)

equalPath :: (RunFailureM m, ErrorM m, ?env :: Env) => Path -> Path -> m ()
equalPath p1 @ (Path v1 tys1) p2 @ (Path v2 tys2)
  | v1 /= v2                   = throwE $ PathMismatch p1 p2
  | length tys1 /= length tys2 = throwE $ PathMismatch p1 p2
  | otherwise = do
    Z run <- runF
    -- TODO: It is perhaps needed to check whether `ty1` and `ty2` have the same kind.
    let f (ty1, ty2) = run $ kindOf (toType ty1) >>= equal (toType ty1) (toType ty2)
    case mapM_ f $ zip tys1 tys2 of
      Right ()             -> return ()
      Left (Failure _ _ _) -> throwE $ PathMismatch p1 p2

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
  -- @applySmall s@ is not always idempotent.
  -- For example, elaboration of "where" or function types involves non-idempotent substitution.
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
      | length ks == length tys -> f ks ty
      | length ks < length tys  -> g (length tys - length ks) ks ty
      | otherwise               -> error $ "ill-formed semantic path: " ++ show (ks, tys)
  where
    f :: [IKind] -> SemanticType -> SemanticType
    f ks ty = shift (-length ks) $ applySmall (fromList $ zip (map variable [0..length ks-1]) $ map parameterized $ reverse $ shift (length ks) $ applySmall s tys) ty

    -- I.Base is used here because we ignore kinds for substitution, so we can choose an arbitrary kind.
    g :: Int -> [IKind] -> SemanticType -> SemanticType
    g n ks (SemanticPath p) = f (replicate n I.Base ++ ks) $ SemanticPath $ appendPath (map (SemanticPath . fromVariable . variable) $ reverse $ take n [0..]) $ shift n p
    g _ _ _                 = error "ill-formed semantic path"

appendPath :: [SemanticType] -> Path -> Path
appendPath tys' (Path v tys) = Path v $ tys ++ tys'

getStructure :: ErrorM m => Position -> SemanticType -> m (Record SemanticType)
getStructure _ (Structure r) = return r
getStructure p ty            = throwE $ NotStructure p ty

class Subtype a where
  type Coercion a

  (<:) :: (RunFailureM m, ErrorM m, ?env :: Env) => a -> a -> m (Coercion a)

instance Subtype Purity where
  type Coercion Purity = ()

  _ <: Impure    = return ()
  Pure <: Pure   = return ()
  Impure <: Pure = throwE NotSubpurity

instance Subtype SemanticType where
  -- The returned term may not have free term variables, but may have free type variables.
  type Coercion SemanticType = Term

  BaseType b1 <: BaseType b2
    | b1 == b2  = return $ I.Abs (I.BaseType b1) $ var 0
    | otherwise = throwE $ NotSubtype (BaseType b1) (BaseType b2)
  SemanticPath p1 <: SemanticPath p2     = equalPath p1 p2 $> I.Abs (toType p1) (var 0)
  AbstractType aty1 <: AbstractType aty2 = aty1 <: aty2 $> I.Abs (toType $ AbstractType aty1) (toTerm aty2)
  Structure r1 <: Structure r2           = I.Abs (toType r1) . I.TmRecord <$> I.iter f r2
    where
      f :: (RunFailureM m, ErrorM m) => I.Label -> SemanticType -> m Term
      f l ty2 = do
        ty1 <- maybe (throwE $ MissingLabel "subtyping on structures" l $ I.labels r1) return $ projRecord l r1
        t <- ty1 <: ty2
        return $ I.App t $ var 0 `I.Proj` l
  Function u1 <: Function u2 = do
    let (Fun ty1 p1 aty1) = getBody u1
    let (Fun ty2 p2 aty2) = getBody u2
    let ?env = I.insertTypes $ reverse $ getAnnotatedKinds u2
    p1 <: p2
    (t1, tys) <- ty2 `match'` shift (qsLen u2) (quantify (getAnnotatedKinds u1) ty1)
    t2 <- aty1 <: aty2
    return $ I.Abs (toType u1) $ I.poly (getKinds u2) $ I.Abs (toType ty2) $ I.App t2 $ I.App (I.inst (var 1) tys) $ I.App t1 $ var 0
  Wrapped aty1 <: Wrapped aty2 = do
    Z run <- runF
    case run $ equal (toType aty1) (toType aty2) I.Base of
      Right ()             -> return $ I.Abs (toType aty1) $ var 0
      Left (Failure _ _ _) -> throwE $ NotSubtype (Wrapped aty1) (Wrapped aty2)
  ty1 <: ty2 = throwE $ NotSubtype ty1 ty2

instance Subtype AbstractType where
  -- The returned term may not have free term variables, but may have free type variables.
  type Coercion AbstractType = Term

  aty1 <: aty2 = do
    let ?env = insertTypes $ reverse $ getAnnotatedKinds aty1
    (t, tys) <- match' (getBody aty1) $ shift (qsLen aty1) aty2
    return $ I.Abs (toType aty1) $ I.unpack Nothing (var 0) (qsLen aty1) $ I.pack (I.App t $ var 0) tys (getKinds aty2) $ toType (getBody aty2)

-- Assumes both arguments are in the same context.
-- The returned term may not have free term variables, but may have free type variables.
match :: (RunFailureM m, ErrorM m, ?env :: Env) => SemanticType -> AbstractType -> m (Term, [Parameterized])
match ty aty = do
  tys <- either (throwE . NoRealization ty aty) return $ lookupInsts (enumVars aty) (shift (qsLen aty) ty) (getBody aty)
  t <- (ty <: shift (-qsLen aty) (applySmall (fromList $ zip (enumVars aty) tys) $ getBody aty)) `catchE` (throwE . NotMatch ty aty)
  return (t, shift (-qsLen aty) tys)

-- Assumes both arguments are in the same context.
-- The returned term may not have free term variables, but may have free type variables.
match' :: (RunFailureM m, ErrorM m, ?env :: Env) => SemanticType -> AbstractType -> m (Term, [IType])
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

-- In the body, the variable 0 denotes the argument which is lastly passed to.
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
    ps <- lookupInsts (enumVars u1) (shift (qsLen u1) $ domain $ getBody u2) (domain $ getBody $ shift (qsLen u2) u1)
    let s = fromList $ zip (map variable [0..]) ps
    mfty <- lookupInst (appendPath (map (SemanticPath . fromVariable) $ enumVars u2) $ shift (qsLen u2) p)
                       (shift (-qsLen u1) $ applySmall s $ shiftAbove (qsLen u1) (qsLen u2) $ getBody $ codomain $ getBody u1)
                       (getBody $ codomain $ getBody u2)
    return $ fmap (tabs $ getKinds u2) <$> mfty
lookupInst _ _ _ = return Nothing

-- Maybe assumes all of 3 arguments are in the same context.
lookupInsts :: [Variable] -> SemanticType -> SemanticType -> LError [Parameterized]
lookupInsts vs ty1 ty2 = fst <$> foldrM f ([], ty2) vs
  where
    -- If `ty1` does not contain type variables less than @length vs@, @applySmall [(v, r)]@ will be idempotent.
    f :: Variable -> ([Parameterized], SemanticType) -> LError ([Parameterized], SemanticType)
    f v (tys, ty) = (\r -> (r : tys, applySmall [(v, r)] ty)) <$> res
      where
        res :: LError Parameterized
        res = lookupInst (fromVariable v) ty1 ty >>= (coerce . maybe (Left v) Right)

class Elaboration a where
  type Output a
  type Effs a (m :: * -> *) :: Constraint

  elaborate :: (Effs a m, ?env :: Env) => Positional a -> m (Output a)

class Monad m => ErrorM m where
  throwE :: ElaborateError -> m a
  catchE :: m a -> (ElaborateError -> m a) -> m a

class Monad m => FreshM m where
  -- Generates a new name in the form: "?d28".
  freshName :: m Name
  freshGenerated :: m Generated

pattern EmptyExistential1 :: a -> Existential a
pattern EmptyExistential1 x <- Existential (Quantified ([], x))

instance Elaboration Type where
  type Output Type = AbstractType
  type Effs Type m = (FailureM m, ErrorM m, FreshM m, PrimAM m, RunFailureM m)

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
        let s = fromList $ a ++ b -- @applySmall s@ is not idempotent.
        let aty2' = quantify (fmap (\k -> foldr I.KFun k $ getKinds aty1) <$> getAnnotatedKinds aty2) $ applySmall s $ getBody aty2
        return $ qmap (\ty2 -> Function $ qmap (\ty1 -> Fun ty1 Pure $ fromBody ty2) $ toUniversal aty1') aty2'
  elaborate (Positional p (Expr e)) = do
    z <- elaborate $ positional p e
    case z of
      (_, _, Impure)                               -> throwE $ ImpureType e
      (_, EmptyExistential1 (AbstractType aty), _) -> return aty
      (_, EmptyExistential1 ty, _)                 -> throwE $ NotReifiedType p ty e
      (_, aty, _)                                  -> throwE $ NotEmptyExistential aty
  elaborate (Positional _ (Singleton e)) = do
    withExplicitType e
    z <- elaborate e
    case z of
      (_, _, Impure)               -> throwE $ ImpureType $ fromPositional e
      (_, EmptyExistential1 ty, _) -> return $ fromBody ty
      (_, _, _)                    -> error "in the absence of weak sealing, a pure expression must not be given an existential type"
  elaborate (Positional _ (Sig ds)) = do
    (aty, _) <- foldlM elaborateDecls (fromBody [], ?env) ds
    return $ Structure <$> aty
  elaborate (Positional p (Where ty1 ids ty2)) = do
    aty1 <- elaborate ty1
    aty2 <- elaborate ty2
    ty <- proj p (getBody aty1) ids
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
    -- TODO: This might be wrong: shift needed?
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

proj :: ErrorM m => Position -> SemanticType -> [Ident] -> m SemanticType
proj _ ty []                    = return ty
proj p (Structure r) (id : ids) =
  let l = toLabel $ coerce id in
  case projRecord l r of
    Just ty -> proj p ty ids
    Nothing -> throwE $ MissingLabel "meta-level projection" l $ I.labels r
proj p ty _ = throwE $ NotStructure p ty

elaborateDecls :: Effs Decl m => (Existential (Record SemanticType), Env) -> Positional Decl -> m (Existential (Record SemanticType), Env)
elaborateDecls (acc, env) d = do
  let ?env = env
  aty <- elaborate d
  let ?env = I.insertTypes $ reverse $ getAnnotatedKinds aty
  let ?env = foldl (\env (l, ty) -> let ?env = env in insertValue (toName l) ty) ?env $ I.toList $ getBody aty
  mustBeDisjoint (getPosition d) acc aty
  return (merge acc aty, ?env)

mustBeDisjoint :: ErrorM m => Position -> Existential (Record SemanticType) -> Existential (Record SemanticType) -> m ()
mustBeDisjoint p e1 e2 =
  let s = I.intersection (getBody e1) $ getBody e2 in
  if Set.null s
    then return ()
    else throwE $ DuplicateSpec p s

arrowsP :: [Param] -> Positional Type -> Positional Type
arrowsP ps ty = foldr f ty ps
  where
    f :: Param -> Positional Type -> Positional Type
    f (Param id ty1) ty2 = connecting id ty2 $ arrowP (return $ fromPositional id) ty1 ty2 -- Perhaps `Param` data constructor should have its position including surrounding parentheses.
    f (Omit id) ty       = connecting id ty $ arrowP (return $ fromPositional id) (positional (getPosition id) TypeType) ty

instance Elaboration Decl where
  type Output Decl = Existential (Record SemanticType)
  type Effs Decl m = (FailureM m, ErrorM m, FreshM m, PrimAM m, RunFailureM m)

  elaborate (Positional p (Spec id ps ty)) =
    case ps of
      [] -> qmap (\ty -> [(toLabel $ coerce id, ty)]) <$> elaborate ty
      _  -> elaborate $ positional p $ spec id $ arrowsP ps ty
  elaborate (Positional p (AbsTypeSpec id ps))    = elaborate $ positional p $ spec id $ arrowsP ps $ positional p TypeType -- TODO: Handle positions correctly.
  elaborate (Positional p (ManTypeSpec id ps ty)) = elaborate $ positional p $ spec id $ arrowsP ps $ positional (getPosition ty) (Singleton $ positional (getPosition ty) $ Type ty) -- TODO: Handle positions correctly.
  elaborate (Positional _ (DInclude ty))          = elaborate ty >>= qfmap (getStructure $ getPosition ty)

withExplicitType :: ErrorM m => Positional Expr -> m ()
withExplicitType (Positional p e) =
  case e of
    Seal _ _           -> return ()
    TransparentAsc _ _ -> return ()
    Type _             -> return ()
    _                  -> throwE $ MissingExplicitType p e

instance Elaboration Literal where
  type Output Literal = BaseType
  type Effs Literal m = Applicative m

  elaborate = pure . I.typeOfLiteral . fromPositional

mustBePure :: ErrorM m => Purity -> m ()
mustBePure Pure   = return ()
mustBePure Impure = throwE NotPure

strongSealing :: AbstractType -> Purity
strongSealing aty
  | qsLen aty == 0 = Pure
  | otherwise      = Impure

instance Elaboration Expr where
  type Output Expr = (Term, AbstractType, Purity)
  type Effs Expr m = (FailureM m, ErrorM m, FreshM m, PrimAM m, RunFailureM m)

  elaborate (Positional pos (Lit l)) = do
    b <- elaborate $ Positional pos l
    return (I.Lit l, fromBody $ BaseType b, Pure) -- Literals are always pure.
  elaborate (Positional p (Id id)) = do
    (ty, v) <- lookupValueByName p $ coerce id
    return (v, fromBody ty, Pure)
  elaborate (Positional _ (Struct bs)) = do
    (_, aty, zs, p) <- foldlM elaborateBindings (?env, fromBody mempty, [], Pure) bs
    let lls = map (\(_, _, ls) -> ls) zs
    let t = I.TmRecord $ Record $ buildRecord lls
    let t1 = I.pack t (I.TVar <$> enumVars aty) (getKinds aty) $ toType $ shift (qsLen aty) $ getBody aty
    t <- foldlM joinBindings t1 zs
    return (t, Structure <$> aty, p)
  elaborate (Positional _ (Type ty)) = do
    aty <- elaborate ty
    return (toTerm aty, fromBody $ AbstractType aty, Pure)
  elaborate (Positional p (Seal e ty)) = do
    case fromPositional e of
      Id _ -> do
        (t1, aty1, p) <- elaborate e
        mustBePure p -- Variables have always the pure effect.
        aty2 <- elaborate ty
        (t2, tys) <- match' (getBody aty1) aty2 -- @aty1@ has always no quantifiers.
        return (I.pack (I.App t2 t1) tys (getKinds aty2) (toType $ getBody aty2), aty2, strongSealing aty2)
      _ -> do
        id <- coerce <$> freshName
        -- TODO: Handle positions correctly.
        elaborate $ positional p $ Let [positional (getPosition e) $ val id e] $ positional p $ Seal (positional (getPosition e) $ Id id) ty
  elaborate (Positional p (TransparentAsc e ty)) = do
    -- We choose an arbitrary identifier.
    let id = positional (getPosition e) $ ident "X"
    -- TODO: Handle positions correctly.
    elaborate $ positional p $ App (positional (getPosition ty) $ Abs (Param id ty :| []) $ Id <$> id) e
  elaborate (Positional p (Abs (param :| params) e)) = do
    case params of
      -- TODO: Handle positions correctly.
      param1 : ps -> elaborate $ positional p $ Abs (param :| []) $ positional p $ Abs (param1 :| ps) e
      [] ->
        case param of
          Omit id -> do
            -- Assumes the `type` type is omitted.
            aty1 <- elaborate $ positional (getPosition id) TypeType
            let ?env = insertTypes $ reverse $ getAnnotatedKinds aty1
            let ?env = insertValue (coerce $ fromPositional id) $ getBody aty1
            (t, aty2, p) <- elaborate e
            return (I.poly (getKinds aty1) $ I.Abs (toType $ getBody aty1) $ t, fromBody $ Function $ qmap (\ty -> Fun ty p aty2) $ toUniversal aty1, Pure)
          Param id ty -> do
            aty1 <- elaborate ty
            let ?env = insertTypes $ reverse $ getAnnotatedKinds aty1
            let ?env = insertValue (coerce $ fromPositional id) $ getBody aty1
            (t, aty2, p) <- elaborate e
            return (I.poly (getKinds aty1) $ I.Abs (toType $ getBody aty1) $ t, fromBody $ Function $ qmap (\ty -> Fun ty p aty2) $ toUniversal aty1, Pure)
  elaborate (Positional p (App e1 e2)) = do
    case (fromPositional e1, fromPositional e2) of
      (Id _, Id _) -> do
        (t1, aty1, _) <- elaborate e1
        (t2, aty2, _) <- elaborate e2
        case getBody aty1 of
          Function u -> do
            (t3, tys) <- match (getBody aty2) $ toExistential $ qmap domain u
            return (I.App (I.inst t1 $ toType <$> tys) $ I.App t3 t2, shift (-qsLen u) $ applySmall (fromList $ zip (enumVars u) $ shift (qsLen u) tys) $ codomain $ getBody u, getPurity $ getBody u)
          ty1 -> throwE $ NotFunction (getPosition e1) ty1
      _ -> do
        id1 <- coerce <$> freshName
        id2 <- coerce <$> freshName
        -- TODO: Handle positions correctly.
        elaborate $ positional p $ Let [positional (getPosition e1) $ val id1 e1, positional (getPosition e2) $ val id2 e2] $ positional p $ App (positional (getPosition e1) $ Id id1) (positional (getPosition e2) $ Id id2)
  elaborate (Positional _ (Proj e id)) = do
    (t, aty, p) <- elaborate e
    r <- getStructure (getPosition e) $ getBody aty
    let l = toLabel $ coerce id
    ty <- maybe (throwE $ MissingLabel "elaborating projection" l $ I.labels r) return $ projRecord l r
    let aty1 = qmap (const ty) aty
    return (I.unpack Nothing t (qsLen aty) $ I.pack (I.Proj (var 0) l) (I.TVar <$> enumVars aty) (getKinds aty) $ toType $ getBody aty1, aty1, p)
  elaborate (Positional p (If e1 e2 e3 ty)) = do
    case fromPositional e1 of
      Id id -> elaborateIf (positional (getPosition e1) id) e2 e3 ty
      _     -> do
        id <- coerce <$> freshName
        -- TODO: Handle positions correctly.
        elaborate $ positional p $ Let [positional (getPosition e1) $ val id e1] $ positional p $ If (positional (getPosition e1) $ Id id) e2 e3 ty
  elaborate (Positional p (Wrap e ty)) = do
    case fromPositional e of
      Id _ -> do
        (t1, aty1, _) <- elaborate e
        aty2 <- elaborate ty
        case aty2 of
          EmptyExistential1 (Wrapped aty2') -> do
            t2 <- aty1 <: aty2'
            return (I.App t2 t1, aty2, Pure)
          EmptyExistential1 ty' -> throwE $ NotWrappedType (getPosition ty) ty'
          _ -> throwE $ NotEmptyExistential aty2
      _ -> do
        id <- coerce <$> freshName
        -- TODO: Handle positions correctly.
        elaborate $ positional p $ Let [positional (getPosition e) $ val id e] $ positional p $ Wrap (positional (getPosition e) $ Id id) ty
  elaborate (Positional p (Unwrap e ty)) = do
    case fromPositional e of
      Id _ -> do
        (t1, aty1, _) <- elaborate e
        aty1' <- case getBody aty1 of
          Wrapped aty1' -> return aty1'
          ty            -> throwE $ NotWrappedType (getPosition e) ty
        aty2 <- elaborate ty
        case aty2 of
          EmptyExistential1 (Wrapped aty2') -> do
            t2 <- aty1' <: aty2'
            return (I.App t2 t1, aty2', strongSealing aty2')
          EmptyExistential1 ty' -> throwE $ NotWrappedType (getPosition ty) ty'
          _ -> throwE $ NotEmptyExistential aty2
      _ -> do
        id <- coerce <$> freshName
        -- TODO: Handle positions correctly.
        elaborate $ positional p $ Let [positional (getPosition e) $ val id e] $ positional p $ Unwrap (positional (getPosition e) $ Id id) ty
  elaborate (Positional p (Let bs e)) = do
    id <- coerce <$> freshName
    -- TODO: Handle positions correctly.
    elaborate $ positional p $ Proj (positional p $ Struct $ bs ++ [positional p $ val id e]) id
  elaborate (Positional p (LetOp op id ty e1 e2)) = do
    -- TODO: Handle positions correctly.
    let k = App (connecting op ty $ App (Id <$> op) $ positional (getPosition ty) $ Type ty) e1
    elaborate $ positional p $ App (connecting op e1 k) $ positional (getPosition e2) $ Abs [Param id ty] e2
  elaborate (Positional p (OpenE e1 e2)) = do
    -- TODO: Handle positions correctly.
    elaborate $ positional p $ Let [positional (getPosition e1) $ Include e1] e2
  elaborate (Positional p Fix) = do
    id1 <- coerce <$> freshName
    id2 <- coerce <$> freshName
    -- TODO: Handle positions correctly.
    let ty1 = positional p $ arrowI Nothing (positional p $ Expr $ Id id1) $ positional p $ Expr $ Id id2
    ty <- elaborate $ positional p $
                      arrowP (Just id1) (positional p TypeType) $ positional p $
                      arrowP (Just id2) (positional p TypeType) $ positional p $
                      arrowI Nothing (positional p $ arrowI Nothing ty1 ty1) ty1
    let f = SemanticPath . fromVariable
    let t = I.Poly I.Base $ I.Abs (toType $ AbstractType $ fromBody $ f $ variable 0) $
            I.Poly I.Base $ I.Abs (toType $ AbstractType $ fromBody $ f $ variable 0) $
            I.Abs ((tvar 1 +> tvar 0) +> tvar 1 +> tvar 0) $
            I.Abs (tvar 1) $
            I.Fix `I.Inst` tvar 1 `I.Inst` tvar 0 `I.App` var 1 `I.App` var 0
    return (t, ty, Pure)

elaborateIf :: (Effs Expr m , ?env :: Env) => Positional Ident -> Positional Expr -> Positional Expr -> Positional Type -> m (Output Expr)
elaborateIf id e2 e3 ty = do
  (t1, aty1, _) <- elaborate $ Id <$> id
  case getBody aty1 of
    BaseType I.Bool -> do
      aty <- elaborate ty
      (t2, aty2, p2) <- elaborate e2
      (t3, aty3, p3) <- elaborate e3
      f2 <- aty2 <: aty
      f3 <- aty3 <: aty
      return (I.If t1 (I.App f2 t2) $ I.App f3 t3, aty, p2 <> p3 <> strongSealing aty)
    ty -> throwE $ NotBool (getPosition id) ty

buildRecord :: [[(Bool, I.Label)]] -> Map.Map I.Label Term
buildRecord lls = fst $ foldl f (mempty, 0) lls
  where
    f = foldr (\(b, l) (m', n') -> (if b then Map.insertWith (\_ x -> x) l (var n') m' else m', n' + 1))

joinBindings :: FreshM m => Term -> (Term, Int, [(Bool, I.Label)]) -> m Term
joinBindings acc (t, n, ls) = do
  g <- freshGenerated
  return $ I.unpack (Just g) t n $ I.Let (map (I.Proj (I.GVar g) . snd) ls) acc

type IsExport = Bool

type Acc = (Env, Existential (Record SemanticType), [(Term, Int, [(IsExport, I.Label)])], Purity)

elaborateBindings :: (Effs Expr m, ?env :: Env) => Acc -> Positional Binding -> m Acc
elaborateBindings (env, whole_aty, zs, p0) b = do
  let ?env = env
  (t, aty, p) <- elaborate b
  let ?env = I.insertTypes $ reverse $ getAnnotatedKinds aty
  let r = getBody aty
  let ?env = foldl (\env (l, ty) -> let ?env = env in insertValue (toName l) ty) ?env $ I.toList r
  let (exports, ls) = if noReexport $ fromPositional b then (mempty, (,) False <$> I.labels r) else (r, (,) True <$> I.labels r)
  return (?env, merge whole_aty $ quantify (getAnnotatedKinds aty) exports, (t, qsLen aty, ls) : zs, p0 <> p)

merge :: Existential (Record SemanticType) -> Existential (Record SemanticType) -> Existential (Record SemanticType)
merge aty1 aty2 =
  let ty1 = shift (qsLen aty2) $ getBody aty1 in
  quantify (getAnnotatedKinds aty2 ++ getAnnotatedKinds aty1) $ getBody aty2 <> ty1

class Monad m => PrimAM m where
  getATypeOfPrim :: T.Text -> m AbstractType

instance Elaboration Binding where
  type Output Binding = (Term, Existential (Record SemanticType), Purity)
  type Effs Binding m = (FailureM m, ErrorM m, FreshM m, PrimAM m, RunFailureM m)

  elaborate (Positional p (Val id ps asc e)) =
    case (ps, asc) of
      ([], Nothing) -> do
        (t, aty, p) <- elaborate e
        let l = I.toLabel $ coerce id
        return (I.unpack Nothing t (qsLen aty) $ I.pack (I.TmRecord $ record [(l, var 0)]) (I.TVar <$> enumVars aty) (getKinds aty) $ I.TRecord $ (\x -> [(l, x)]) $ toType $ getBody aty, (\x -> record [(l, x)]) <$> aty, p)
      _ -> do
        elaborate $ positional p $ val id $ abs ps $ ascribe asc e
      where
        ascribe Nothing e             = e
        ascribe (Just (Trans, ty)) e  = positional p $ TransparentAsc e ty
        ascribe (Just (Opaque, ty)) e = positional p $ Seal e ty

        abs :: [Param] -> Positional Expr -> Positional Expr
        abs []       e  = e
        abs (p1 : ps) e = positional p $ Abs (p1 :| ps) e

  elaborate (Positional p (TypeBinding id ps ty)) = do
    case ps of
      []           -> elaborate $ positional p $ val id $ positional (getPosition ty) $ Type ty
      (param : ps) -> elaborate $ positional p $ val id $ positional (getPosition ty) $ Abs (param :| ps) $ positional (getPosition ty) $ Type ty

  elaborate (Positional _ (Include e)) = do
    (t, aty, p) <- elaborate e
    r <- getStructure (getPosition e) $ getBody aty
    return (t, quantify (getAnnotatedKinds aty) r, p)

  elaborate (Positional _ (Open e)) = do
    (t, aty, p) <- elaborate e
    r <- getStructure (getPosition e) $ getBody aty
    return (t, quantify (getAnnotatedKinds aty) r, p)

  -- TODO: Handle positions correctly.
  elaborate (Positional p (Local bs1 bs2)) = elaborate $ positional p $ Include $ positional p $ Let bs1 $ positional p $ Struct bs2

  elaborate (Positional _ (Primitive id s)) = do
    aty <- getATypeOfPrim s
    let l = I.toLabel $ coerce id
    return (I.TmRecord $ record [(l, I.Primitive s)], (\x -> record [(l, x)]) <$> aty, Pure)
