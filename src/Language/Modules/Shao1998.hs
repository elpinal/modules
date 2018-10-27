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

import qualified Language.Modules.Shao1998.Semantics as S

data MC

instance S.ModuleCalculus MC where
  type TypeIdent MC = TypeIdent
  type StrIdent MC = StrIdent
  type FctIdent MC = FctIdent
  type Kind MC = Kind
  type DefStr MC = DefStruct
  type TypePath MC = TypePath
  type Spec MC = Spec

type Basis = S.Basis MC

newtype StrIdent = StrIdent Int
  deriving (Eq, Show)

newtype FctIdent = FctIdent Int
  deriving (Eq, Show)

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
  = TypeDecl TypeIdent TyCon
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
  deriving (Eq, Show)

class SigSubsume a where
  sigSubsume :: Members '[State Basis, Error TypeError] r => a -> a -> Eff r ()

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

c2m :: Members '[State Basis, Error TypeError] r => TyCon -> Eff r (S.TyCon Kind)
c2m = evalState ([] :: [TypeIdent]) . c2m'

c2m' :: Members '[State [TypeIdent], State Basis, Error TypeError] r => TyCon -> Eff r (S.TyCon Kind)
c2m' (TyConPath tp @ (TypePath _ tid)) = gets (elemIndex tid) >>= maybe f (return . S.TyConVar)
  where
    f = do
      re <- gets S.realEnv
      case S.lookupTReal tid (re :: S.RealEnv MC) of
        Just tr -> return $ S.getTReal tr
        Nothing -> throwError $ CouldNotRealizeTypePath tp
c2m' TyConInt = return S.TyConInt
c2m' (TyConFun tc1 tc2) = S.TyConFun <$> c2m' tc1 <*> c2m' tc2
c2m' (TyConAbs tid tc)  = modify (tid :) >> S.TyConAbs <$> c2m' tc <* escape
c2m' (TyConApp tc1 tc2) = S.TyConApp <$> c2m' tc1 <*> c2m' tc2

escape :: Member (State [TypeIdent]) r => Eff r ()
escape = modify (tail :: [TypeIdent] -> [TypeIdent])
