{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Modules.Shao1998.Semantics
  ( ModuleCalculus(..)
  , Basis(..)
  , StampEnv(..)
  , SpecEnv(..)
  , RealEnv(..)
  , lookupTReal
  , lookupSReal
  , lookupFReal
  , TReal(..)
  , SReal(..)
  , FReal(..)
  , TyCon(..)
  , tyConStampEq
  ) where

import qualified Data.Map.Lazy as Map

import qualified Language.Modules.Shao1998.Target as T

class
  ( Eq (TypeIdent a)
  , Eq (StrIdent a)
  , Eq (FctIdent a)
  , Eq (Kind a)
  , Eq (DefStr a)
  , Eq (TypePath a)
  , Eq (Spec a)
  , Show (TypeIdent a)
  , Show (StrIdent a)
  , Show (FctIdent a)
  , Show (Kind a)
  , Show (DefStr a)
  , Show (TypePath a)
  , Show (Spec a)
  ) => ModuleCalculus a where
  type TypeIdent a
  type StrIdent a
  type FctIdent a
  type Kind a
  type DefStr a
  type TypePath a
  type Spec a

newtype Stamp = Stamp Int
  deriving (Eq, Ord, Show)

data TyCon k
  = TypeStamp Stamp k T.TyCon
  | TyConVar Int
  | TyConInt
  | TyConFun (TyCon k) (TyCon k)
  | TyConAbs (TyCon k)
  | TyConApp (TyCon k) (TyCon k)
  deriving (Eq, Show)

tyConStampEq :: Eq k => TyCon k -> TyCon k -> Bool
tyConStampEq (TypeStamp s1 _ _) (TypeStamp s2 _ _) = s1 == s2
tyConStampEq tc1 tc2                               = tc1 == tc2

newtype TReal k = TReal { getTReal :: TyCon k }
  deriving (Eq, Show)

newtype SReal mc = SReal (RealEnv mc)
  deriving (Eq, Show)

data FReal mc
  = Closure (DefStr mc) (Basis mc) AuxInfo
  | Template AuxInfo

deriving instance ModuleCalculus mc => Eq (FReal mc)
deriving instance ModuleCalculus mc => Show (FReal mc)

data AuxInfo = AuxInfo T.TyCon T.Type
  deriving (Eq, Show)

data Basis mc = Basis
  { kindEnv :: T.KindEnv
  , stampEnv :: StampEnv (TypePath mc)
  , specEnv :: SpecEnv (Spec mc)
  , realEnv :: RealEnv mc
  }

deriving instance ModuleCalculus mc => Eq (Basis mc)
deriving instance ModuleCalculus mc => Show (Basis mc)

newtype StampEnv tp = StampEnv (Map.Map Stamp tp)
  deriving (Eq, Show, Semigroup, Monoid)

newtype SpecEnv sp = SpecEnv [sp]
  deriving (Eq, Show, Foldable)

data RealEnv mc = RealEnv
  { tReal :: Map.Map (TypeIdent mc) (TReal (Kind mc))
  , sReal :: Map.Map (StrIdent mc) (SReal mc)
  , fReal :: Map.Map (FctIdent mc) (FReal mc)
  }

deriving instance ModuleCalculus mc => Eq (RealEnv mc)
deriving instance ModuleCalculus mc => Show (RealEnv mc)

lookupTReal :: (ModuleCalculus mc, Ord (TypeIdent mc)) => TypeIdent mc -> RealEnv mc -> Maybe (TReal (Kind mc))
lookupTReal tid = Map.lookup tid . tReal

lookupSReal :: (ModuleCalculus mc, Ord (StrIdent mc)) => StrIdent mc -> RealEnv mc -> Maybe (SReal mc)
lookupSReal sid = Map.lookup sid . sReal

lookupFReal :: (ModuleCalculus mc, Ord (FctIdent mc)) => FctIdent mc -> RealEnv mc -> Maybe (FReal mc)
lookupFReal fid = Map.lookup fid . fReal
