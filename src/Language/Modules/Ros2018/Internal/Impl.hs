{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Language.Modules.Ros2018.Internal.Impl
  ( runFailure
  , typecheck
  , equalType
  ) where

import qualified Data.Map.Lazy as Map
import qualified Data.Text as T
import Polysemy
import Polysemy.Error hiding (throw)
import qualified Polysemy.Error as E
import Polysemy.Reader

import Language.Modules.Ros2018.Internal

newtype M a = M (Sem '[Reader (Map.Map T.Text Type), Error Failure] a)
  deriving (Functor, Applicative, Monad)

runFailure :: M a -> Either Failure a
runFailure (M m) = run $ runError $ runReader mempty m

instance FailureM M where
  throwFailure f = M $ E.throw f

instance PrimM M where
  getTypeOfPrim s = do
    m <- M ask
    maybe (throw $ NoSuchPrimitive s) return $ Map.lookup s m

typecheck :: Env Id Type -> Term -> Either Failure Type
typecheck env t = runFailure $ let ?env = env in whTypeOf t

-- Assumes input types have the base kind.
equalType :: Type -> Type -> Either Failure ()
equalType ty1 ty2 = runFailure $ let ?env = emptyEnv :: Env Id Type in equal ty1 ty2 Base
