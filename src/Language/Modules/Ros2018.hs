{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Modules.Ros2018
  (
  -- * Objects
    Ident
  , ident

  -- * Syntax
  , Type(..)
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
  , Path
  , fromVariable

  -- * Instantiation
  , lookupInsts

  -- * Embedding to internal objects
  , ToType(..)

  -- * Quantification
  , Existential
  , Universal
  , Quantification(..)
  ) where

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Error
import Control.Monad.Freer.Fresh
import Data.Coerce
import Data.Foldable
import Data.List
import qualified Data.Map.Lazy as Map
import Data.Maybe
import Data.Monoid hiding (First)
import Data.Semigroup (First(..))
import qualified Data.Text as T
import GHC.Exts
import GHC.Generics

import Language.Modules.Ros2018.Display
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal hiding (Env, Term(..), Type(..), Kind(..))
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

data Type
  = Base BaseType
  | TypeType
  | Arrow (Maybe Ident) (Positional Type) Purity (Positional Type)
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
    showParen (4 <= n) $ dom (fromPositional ty1) . showChar ' ' . showString arr . showChar ' ' . displaysPrec 3 (fromPositional ty2)

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
  displaysPrec _ (Val id e)  = displays id . showString " = " . displays (fromPositional e)
  displaysPrec _ (Include e) = showString "include " . displaysPrec 5 (fromPositional e)

data Expr
  = Lit Literal
  | Id Ident
  | Struct [Positional Binding]
  | Type (Positional Type)
  deriving (Eq, Show)

instance Display Expr where
  displaysPrec _ (Lit l)     = displays l
  displaysPrec _ (Id id)     = displays id
  displaysPrec _ (Struct bs) = showString "struct " . appEndo (mconcat $ coerce $ intersperse (showString "; ") $ map (displays . fromPositional) bs) . showString " end"
  displaysPrec n (Type ty)   = showParen (4 <= n) $ showString "type " . displaysPrec 5 (fromPositional ty)

type Env = I.Env Positional SemanticType

data ElaborateError
  = NotStructure SemanticType
  deriving (Eq, Show)

instance Display ElaborateError where
  display (NotStructure lty) = "not structure type: " ++ display (WithName lty)

data Path = Path Variable [IType]
  deriving (Eq, Show)
  deriving Generic

instance Shift Path

instance DisplayName Path where
  displaysWithName n p = displaysWithName n $ toType p

instance ToType Path where
  toType (Path v tys) = foldl I.TApp (I.TVar v) tys

fromVariable :: Variable -> Path
fromVariable v = Path v []

data Fun = Fun SemanticType Purity AbstractType
  deriving (Eq, Show)
  deriving Generic

instance Shift Fun

instance DisplayName Fun where
  displaysWithName n (Fun ty p aty) =
    showParen (4 <= n) $ displaysWithName 4 ty . showString (" " ++ displayArrow p ++ " ") . displaysWithName 3 aty

isPure :: Fun -> Bool
isPure (Fun _ p _) = p == Pure

codomain :: Fun -> AbstractType
codomain (Fun _ _ aty) = aty

domain :: Fun -> SemanticType
domain (Fun ty _ _) = ty

data SemanticType
  = BaseType BaseType
  | Structure (Record SemanticType)
  | AbstractType AbstractType
  | SemanticPath Path
  | Function (Universal Fun)
  deriving (Eq, Show)
  deriving Generic

instance Shift SemanticType

instance DisplayName SemanticType where
  displaysWithName _ (BaseType b)       = displays b
  displaysWithName _ (Structure r)      = displaysWithName 0 r
  displaysWithName _ (AbstractType aty) = showString "[= " . displaysWithName 0 aty . showString "]"
  displaysWithName n (SemanticPath p)   = displaysWithName n p
  displaysWithName n (Function u)       =
    let ?nctx = newTypes $ qsLen u in
    let f = mconcat $ coerce $ intersperse (showString ", ") $ map (\(i, k) -> displayTypeVariable i . showString " : " . displays k) $ zip [0..] $ getKinds u in
    showParen (4 <= n) $ (\x -> showChar '∀' . appEndo f . showString ". " . displaysWithName 0 x) $ getBody u

instance Substitution SemanticType where
  apply _ ty @ (BaseType _)  = ty
  apply s (Structure r)      = Structure $ apply s <$> r
  apply s (AbstractType aty) = AbstractType $ apply s aty
  apply s (SemanticPath p)   = SemanticPath $ apply s p
  apply s (Function u)       = Function $ apply s u

instance Substitution a => Substitution (Existential a) where
  apply s e =
    let s1 = shift (qsLen e) s in
    qmap (apply s1) e

instance Substitution a => Substitution (Universal a) where
  apply s u =
    let s1 = shift (qsLen u) s in
    qmap (apply s1) u

instance Substitution Fun where
  apply s (Fun ty p aty) = Fun (apply s ty) p (apply s aty)

instance Substitution a => Substitution [a] where
  apply s xs = apply s <$> xs

instance Substitution Path where
  apply s (Path v tys) =
    case lookupSubst v s of
      Nothing -> Path v $ apply s tys
      Just ty -> either (error . display) id $ appendPath (apply s tys) <$> fromType ty

newtype PathFormationError = PathFromType IType
  deriving (Eq, Show)

instance Display PathFormationError where
  display (PathFromType ty) = "cannot create well-formed semantic path from: " ++ display ty

fromType :: IType -> Either PathFormationError Path
fromType (I.TVar v)       = return $ fromVariable v
fromType (I.TApp ty1 ty2) = appendPath [ty2] <$> fromType ty1
fromType ty               = Left $ PathFromType ty

appendPath :: [IType] -> Path -> Path
appendPath tys' (Path v tys) = Path v $ tys ++ tys'

getStructure :: Member (Error ElaborateError) r => SemanticType -> Eff r (Record SemanticType)
getStructure (Structure r) = return r
getStructure lty           = throwError $ NotStructure lty

class Sized a where
  isSmall :: a -> Bool

instance Sized SemanticType where
  isSmall (BaseType _)       = True
  isSmall (Structure r)      = all isSmall r
  isSmall (AbstractType aty) = isSmall aty
  isSmall (SemanticPath _)   = True -- maybe
  isSmall (Function u)       = isSmall u

instance Sized Fun where
  isSmall (Fun _ Pure _)      = False
  isSmall (Fun ty Impure aty) = isSmall ty && isSmall aty

newtype Quantified a = Quantified ([Positional IKind], a)
  deriving (Eq, Show)
  deriving Functor

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
    let f = mconcat $ coerce $ intersperse (showString ", ") $ map (\(i, k) -> displayTypeVariable i . showString " : " . displays (fromPositional k)) $ zip [0..] ks in
    showString "∃" . appEndo f . showString ". " . displaysWithName 0 x

newtype Universal a = Universal (Quantified a)
  deriving (Eq, Show)
  deriving Functor
  deriving Quantification
  deriving Shift
  deriving Sized

toUniversal :: Existential a -> Universal a
toUniversal = coerce

type AbstractType = Existential SemanticType

instance ToType AbstractType where
  toType (Existential (Quantified (ks, lty))) = I.some (map fromPositional ks) $ toType lty

instance ToType a => ToType (Universal a) where
  toType u = I.forall (getKinds u) $ toType $ getBody u

data Purity
  = Pure
  | Impure
  deriving (Eq, Show)
  deriving Shift via Fixed Purity

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

instance ToType a => ToType (Record a) where
  toType r = I.TRecord $ toType <$> r

toTerm :: AbstractType -> Term
toTerm aty = I.Abs (toType aty) $ I.TmRecord $ record []

lookupInst :: Path -> SemanticType -> SemanticType -> Maybe (First IType)
lookupInst p1 ty (SemanticPath p2)
  | p1 == p2 && isSmall ty = Just $ First $ toType ty
  | otherwise              = Nothing
lookupInst p (Structure r1) (Structure r2) = I.foldMapIntersection (lookupInst p) r1 r2
lookupInst p (Function u1) (Function u2)
  | isPure (getBody u1) && isPure (getBody u2) =
    let s = fromList $ zip (map variable [0..]) $ lookupInsts (enumVars u1) (domain $ getBody u2) (domain $ getBody u1) in
    let mfty = lookupInst (appendPath (map I.TVar $ enumVars u2) p) (apply s $ getBody $ codomain $ getBody u1) (getBody $ codomain $ getBody u2) in
    fmap (I.tabs $ getKinds u2) <$> mfty
lookupInst _ _ _ = Nothing

lookupInsts :: [Variable] -> SemanticType -> SemanticType -> [IType]
lookupInsts vs ty1 ty2 = fst $ foldr f ([], ty2) vs
  where
    f :: Variable -> ([IType], SemanticType) -> ([IType], SemanticType)
    f v (tys, ty) = (res : tys, apply [(v, res)] ty)
      where res = coerce $ fromMaybe (error "not explicit") $ lookupInst (fromVariable v) ty1 ty

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
      Pure -> error "not yet implented: pure function type"

instance Elaboration Literal where
  type Output Literal = BaseType
  type Effs Literal = '[]

  elaborate = return . I.typeOfLiteral . fromPositional

instance Elaboration Expr where
  type Output Expr = (Term, AbstractType, Purity)
  type Effs Expr = '[Error I.Failure, Error ElaborateError, Fresh]

  elaborate (Positional pos (Lit l)) = do
    b <- elaborate $ Positional pos l
    return (I.Lit l, fromBody $ BaseType b, Pure) -- Literals are always pure.
  elaborate (Positional _ (Id id)) = do
    (lty, v) <- lookupValueByName $ coerce id
    return (I.Var v, fromBody lty, Pure)
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
  let ?env = foldl (\env (l, lty) -> let ?env = env in insertValue (toName l) lty) ?env $ I.toList r
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
    return (I.unpack Nothing t (qsLen aty) $ I.pack (I.TmRecord $ record [(l, var 0)]) (I.TVar <$> enumVars aty) (getKinds aty) $ toType $ getBody aty, (\x -> record [(l, x)]) <$> aty, p)

  elaborate (Positional _ (Include e)) = do
    (t, aty, p) <- elaborate e
    r <- getStructure $ getBody aty
    return (t, quantify (getAnnotatedKinds aty) r, p)
