{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Language.Modules.Ros2018.Internal.Impl
  ( runFailure
  , typecheck
  , equalType
  ) where

import Polysemy
import Polysemy.Error

import Language.Modules.Ros2018.Internal

newtype M a = M (Sem '[Error Failure] a)
  deriving (Functor, Applicative, Monad)

runFailure :: M a -> Either Failure a
runFailure (M m) = run $ runError m

instance FailureM M where
  throwFailure f = M $ throw f

typecheck :: Env Id Type -> Term -> Either Failure Type
typecheck env t = runFailure $ let ?env = env in whTypeOf t

-- Assumes input types have the base kind.
equalType :: Type -> Type -> Either Failure ()
equalType ty1 ty2 = runFailure $ let ?env = emptyEnv :: Env Id Type in equal ty1 ty2 Base
