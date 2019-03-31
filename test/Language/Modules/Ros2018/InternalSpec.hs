{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module Language.Modules.Ros2018.InternalSpec where

import Test.Hspec

import Control.Monad.Freer
import Control.Monad.Freer.Error

import Language.Modules.Ros2018.Internal

shouldBeEnvError :: (HasCallStack, Show a) => Either Failure a -> EnvError -> Expectation
shouldBeEnvError (Left (Failure err EvidEnv _)) expected = err `shouldBe` expected
shouldBeEnvError (Right x) _                             = expectationFailure $ "unexpectedly Right value: " ++ show x

shouldBeRight :: (HasCallStack, Eq a, Show a) => Either Failure a -> a -> Expectation
shouldBeRight (Left (Failure err _ f)) _ = expectationFailure $ "error: " ++ f err
shouldBeRight (Right x) expected         = x `shouldBe` expected

spec :: Spec
spec = do
  describe "lookupType" $
    it "look up information about a type variable in an environment" $ do
      let ?env = emptyEnv :: Env [] ()
      run (runError $ lookupType $ variable 0) `shouldBeEnvError` UnboundTypeVariable (variable 0)

      let ?env = insertType [Base]
      run (runError $ lookupType $ variable 0) `shouldBeRight` [Base]

      run (runError $ lookupType $ variable 1) `shouldBeEnvError` UnboundTypeVariable (variable 1)
