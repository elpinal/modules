{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module Language.Modules.Ros2018.InternalSpec where

import Test.Hspec

import Control.Monad.Freer
import Control.Monad.Freer.Error

import Language.Modules.Ros2018.Internal
import Language.Modules.Ros2018.Shift

shouldBeEnvError :: (HasCallStack, Show a) => Either Failure a -> EnvError -> Expectation
shouldBeEnvError (Left (Failure err EvidEnv _)) expected = err `shouldBe` expected
shouldBeEnvError (Right x) _                             = expectationFailure $ "unexpectedly Right value: " ++ show x

shouldBeRight :: (HasCallStack, Eq a, Show a) => Either Failure a -> a -> Expectation
shouldBeRight (Left (Failure err _ f)) _ = expectationFailure $ "error: " ++ f err
shouldBeRight (Right x) expected         = x `shouldBe` expected

tvar :: Int -> Type
tvar = TVar . variable

spec :: Spec
spec = do
  describe "lookupType" $
    it "look up information about a type variable in an environment" $ do
      let ?env = emptyEnv :: Env [] ()
      run (runError $ lookupType $ variable 0) `shouldBeEnvError` UnboundTypeVariable (variable 0)

      let ?env = insertType [Base]
      run (runError $ lookupType $ variable 0) `shouldBeRight` [Base]

      run (runError $ lookupType $ variable 1) `shouldBeEnvError` UnboundTypeVariable (variable 1)

  describe "shift" $
    it "shifts type variables" $ do
      shift 1 (variable 0)      `shouldBe` variable 1
      shift 21 (variable 0)     `shouldBe` variable 21
      shift 21 (variable 10)    `shouldBe` variable 31
      shift 128 (variable 8110) `shouldBe` variable 8238

      shift (-1) (variable 0)  `shouldBe` variable (-1)
      shift (-5) (variable 0)  `shouldBe` variable (-5)
      shift (-5) (variable 3)  `shouldBe` variable (-2)
      shift (-6) (variable 6)  `shouldBe` variable 0
      shift (-4) (variable 13) `shouldBe` variable 9

      shift 0 (variable 0) `shouldBe` variable 0
      shift 0 (variable 7) `shouldBe` variable 7

      shift 0 (Forall Base $ BaseType Bool) `shouldBe` Forall Base (BaseType Bool)
      shift 4 (Forall Base $ BaseType Bool) `shouldBe` Forall Base (BaseType Bool)
      shift 4 (Forall Base $ tvar 0)        `shouldBe` Forall Base (tvar 0)
      shift 4 (Forall Base $ tvar 1)        `shouldBe` Forall Base (tvar 5)
      shift (-1) (Forall Base $ tvar 2)     `shouldBe` Forall Base (tvar 1)
      shift (-1) (Forall Base $ tvar 1)     `shouldBe` Forall Base (tvar 0)
      shift (-1) (Forall Base $ tvar 0)     `shouldBe` Forall Base (tvar 0)
      shift (-2) (Forall Base $ tvar 1)     `shouldBe` Forall Base (tvar (-1))
