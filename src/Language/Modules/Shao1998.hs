{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Modules.Shao1998
  (
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Data.List
import qualified Data.Map.Lazy as Map

import qualified Language.Modules.Shao1998.Semantics as S
import qualified Language.Modules.Shao1998.Target as T

data MC

instance S.ModuleCalculus MC where
  type TypeIdent MC = TypeIdent
  type StrIdent MC = StrIdent
  type FctIdent MC = FctIdent
  type Kind MC = Kind
  type DefStr MC = DefStruct
  type TypePath MC = TypePath
  type Spec MC = Spec

type SReal = S.SReal MC
type FReal = S.FReal MC
type RealEnv = S.RealEnv MC
type SpecEnv = S.SpecEnv Spec
type StampEnv = S.StampEnv TypePath

newtype StrIdent = StrIdent Int
  deriving (Eq, Ord, Show)

newtype FctIdent = FctIdent Int
  deriving (Eq, Ord, Show)

newtype TypeIdent = TypeIdent Int
  deriving (Eq, Ord, Show)

data StrPath = StrPath [StrIdent] StrIdent
  deriving (Eq, Show)

data FctPath = FctPath (Maybe StrPath) FctIdent
  deriving (Eq, Show)

data TypePath = TypePath (Maybe StrPath) TypeIdent
  deriving (Eq, Show)

data Kind
  = Mono
  | KFun Kind
  deriving (Eq, Show)

data TyCon
  = TyConPath TypePath
  | TyConInt
  | TyConFun TyCon TyCon
  | TyConAbs TypeIdent TyCon
  | TyConApp TyCon TyCon
  deriving (Eq, Show)

data Spec
  = AbsTypeDef TypeIdent Kind
  | ManTypeDef TypeIdent Kind TyCon
  | StrDef StrIdent Sig
  | FctDef FctIdent FSig
  deriving (Eq, Show)

type Sig = Sig' Spec

newtype Sig' a = Sig { getSig :: [a] }
  deriving (Eq, Show)
  deriving (Functor, Applicative) via ZipList

data FSig = FSig StrIdent Sig Sig
  deriving (Eq, Show)

data Decl
  = TypeDecl TypeIdent Kind TyCon
  | StrDecl StrIdent Struct
  | FctDecl FctIdent Fct
  | Local [Decl] [Decl]
  deriving (Eq, Show)

data Struct
  = StrP StrPath
  | FctApp FctIdent StrIdent
  | Asc StrIdent Sig
  | DefStr DefStruct
  deriving (Eq, Show)

newtype DefStruct = DefStruct [Decl]
  deriving (Eq, Show)

data Fct
  = FctP FctPath
  | DefFct StrIdent Sig DefStruct
  deriving (Eq, Show)

data TypeError
  = SigLengthMismatch Sig Sig
  | TypeDefMismatch Spec Spec
  | ManTypeDefMismatch TyCon TyCon
  | StrDefIdentMismatch StrIdent StrIdent
  | FctDefMismatch Spec Spec
  | CouldNotRealizeTypePath TypePath
  | ComponentMismatch Spec Spec
  | IllegalKindApplication Kind Kind
  | KindMismatch Kind Kind
  | NotMono Kind
  | NonEmptyStampEnv StampEnv
  deriving (Eq, Show)

class SigSubsume a where
  sigSubsume :: Members '[State RealEnv, Error TypeError] r => a -> a -> Eff r ()

instance SigSubsume Sig where
  sigSubsume s @ (Sig xs) s' @ (Sig ys)
    | length xs == length ys = sequence_ $ getSig $ sigSubsume <$> s <*> s'
    | otherwise              = throwError $ SigLengthMismatch s s'

instance SigSubsume Spec where
  sigSubsume s @ (AbsTypeDef tid k)    s' @ (AbsTypeDef tid' k')     = defSubsume (tid, k) (tid', k') $ TypeDefMismatch s s'
  sigSubsume s @ (ManTypeDef tid k _)  s' @ (AbsTypeDef tid' k')     = defSubsume (tid, k) (tid', k') $ TypeDefMismatch s s'
  sigSubsume s @ (ManTypeDef tid k tc) s' @ (ManTypeDef tid' k' tc') = do
    defSubsume (tid, k) (tid', k') $ TypeDefMismatch s s'
    tcm <- c2m tc
    tcm' <- c2m tc'
    unless (tcm `S.tyConStampEq` tcm') $
      throwError $ ManTypeDefMismatch tc tc'
  sigSubsume (StrDef sid s) (StrDef sid' s')
    | sid == sid' = sigSubsume s s'
    | otherwise   = throwError $ StrDefIdentMismatch sid sid'
  sigSubsume s @ (FctDef fid f) s' @ (FctDef fid' f') = defSubsume (fid, f) (fid', f') $ FctDefMismatch s s'
  sigSubsume s s'                                     = throwError $ ComponentMismatch s s'

defSubsume :: (Member (Error TypeError) r, Eq a, Eq b) => (a, b) -> (a, b) -> TypeError -> Eff r ()
defSubsume (x, y) (x', y') e
    | x == x' && y == y' = return ()
    | otherwise          = throwError e

c2m :: Members '[State RealEnv, Error TypeError] r => TyCon -> Eff r (S.TyCon Kind)
c2m = evalState ([] :: [TypeIdent]) . c2m'

c2m' :: Members '[State [TypeIdent], State RealEnv, Error TypeError] r => TyCon -> Eff r (S.TyCon Kind)
c2m' (TyConPath tp @ (TypePath _ tid)) = gets (elemIndex tid) >>= maybe f (return . S.TyConVar)
  where
    f = do
      re <- get
      case S.lookupTReal tid (re :: RealEnv) of
        Just tr -> return $ S.getTReal tr
        Nothing -> throwError $ CouldNotRealizeTypePath tp
c2m' TyConInt = return S.TyConInt
c2m' (TyConFun tc1 tc2) = S.TyConFun <$> c2m' tc1 <*> c2m' tc2
c2m' (TyConAbs tid tc)  = modify (tid :) >> S.TyConAbs <$> c2m' tc <* escape
c2m' (TyConApp tc1 tc2) = S.TyConApp <$> c2m' tc1 <*> c2m' tc2

escape :: Member (State [TypeIdent]) r => Eff r ()
escape = modify (tail :: [TypeIdent] -> [TypeIdent])

transDecl :: Members '[State RealEnv, State SpecEnv, State StampEnv, Error TypeError] r => Decl -> Eff r T.Decl
transDecl (TypeDecl tid k tc) = do
  tcm <- c2m tc
  k' <- kindOf tcm
  if k /= k'
    then throwError $ KindMismatch k k'
    else do
      modify $ realize tid $ S.TReal tcm
      modify $ \(S.SpecEnv xs) -> S.SpecEnv $ ManTypeDef tid k tc : xs
      return mempty
transDecl (StrDecl sid s) = do
  (t, stenv, sig, r) <- transModule s
  modify $ realize sid r
  modify $ \(S.SpecEnv xs) -> S.SpecEnv $ StrDef sid sig : xs
  modify (stenv <>) -- FIXME: lift type-paths with `sid`
  return $ T.Decl [t]
transDecl (FctDecl fid f) = do
  (t, stenv, fsig, r) <- transModule f
  unless (stenv == mempty) $
    throwError $ NonEmptyStampEnv stenv
  modify $ realize fid r
  modify $ \(S.SpecEnv xs) -> S.SpecEnv $ FctDef fid fsig : xs
  return $ T.Decl [t]

-- | Obtains the kind of a (possibly open) semantic type constructor.
kindOf :: Member (Error TypeError) r => S.TyCon Kind -> Eff r Kind
kindOf (S.TypeStamp _ k _)  = return k
kindOf (S.TyConVar _)       = return Mono
kindOf S.TyConInt           = return Mono
kindOf (S.TyConFun tc1 tc2) = mono tc1 >> mono tc2 >> return Mono
kindOf (S.TyConAbs tc)      = KFun <$> kindOf tc
kindOf (S.TyConApp tc1 tc2) = do
  k1 <- kindOf tc1
  k2 <- kindOf tc2
  case k1 of
    Mono -> throwError $ IllegalKindApplication k1 k2
    KFun k
      | k2 == Mono -> return k
      | otherwise  -> throwError $ IllegalKindApplication k1 k2

mono :: Member (Error TypeError) r => S.TyCon Kind -> Eff r ()
mono tc = do
  k <- kindOf tc
  unless (k == Mono) $
    throwError $ NotMono k

class Realize i where
  type Realizer i
  realize :: i -> Realizer i -> RealEnv -> RealEnv

instance Realize TypeIdent where
  type Realizer TypeIdent = S.TReal Kind

  realize tid tr re = re { S.tReal = Map.insert tid tr $ S.tReal re }

instance Realize StrIdent where
  type Realizer StrIdent = SReal

  realize sid sr re = re { S.sReal = Map.insert sid sr $ S.sReal re }

instance Realize FctIdent where
  type Realizer FctIdent = FReal

  realize fid fr re = re { S.fReal = Map.insert fid fr $ S.fReal re }

class TransModule m where
  type TransModuleSig m
  type TransModuleReal m

  transModule :: m -> Eff r (T.Term, StampEnv, TransModuleSig m, TransModuleReal m)

instance TransModule Struct where
  type TransModuleSig Struct = Sig
  type TransModuleReal Struct = SReal

  transModule = undefined

instance TransModule Fct where
  type TransModuleSig Fct = FSig
  type TransModuleReal Fct = FReal

  transModule = undefined
