{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Modules.Shao1998.Semantics
  ( ModuleCalculus(..)
  , Basis(..)
  , RealEnv
  , lookupTReal
  , TReal(..)
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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

newtype SpecEnv sp = SpecEnv sp
  deriving (Eq, Show)

data RealEnv mc = RealEnv
  { t :: Map.Map (TypeIdent mc) (TReal (Kind mc))
  , s :: Map.Map (StrIdent mc) (SReal mc)
  , f :: Map.Map (FctIdent mc) (FReal mc)
  }

deriving instance ModuleCalculus mc => Eq (RealEnv mc)
deriving instance ModuleCalculus mc => Show (RealEnv mc)

lookupTReal :: (ModuleCalculus mc, Ord (TypeIdent mc)) => TypeIdent mc -> RealEnv mc -> Maybe (TReal (Kind mc))
lookupTReal tid re = Map.lookup tid $ t re
