{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}

module Language.Modules.Ros2018.InternalSpec where

import Test.Hspec

import Control.Monad.Freer
import Control.Monad.Freer.Error

import Language.Modules.Ros2018.Display
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

  describe "insertType" $
    it "inserts a new type variable into an environment" $ do
      let ?env = emptyEnv :: Env [] Type
      let ?env = insertType [Base]
      let ?env = insertValue (name "x") $ BaseType Char
      run (runError $ lookupValueByName $ name "x") `shouldBeRight` BaseType Char

      let ?env = insertValue (name "y") $ tvar 0
      run (runError $ lookupValueByName $ name "y") `shouldBeRight` tvar 0

      let ?env = insertValue (name "z") $ tvar 0
      run (runError $ lookupValueByName $ name "y") `shouldBeRight` tvar 0
      run (runError $ lookupValueByName $ name "z") `shouldBeRight` tvar 0

      let ?env = insertType [Base]
      run (runError $ lookupValueByName $ name "y") `shouldBeRight` tvar 1
      run (runError $ lookupValueByName $ name "z") `shouldBeRight` tvar 1

      let ?env = insertValue (name "v") $ tvar 0
      run (runError $ lookupValueByName $ name "x") `shouldBeRight` BaseType Char
      run (runError $ lookupValueByName $ name "y") `shouldBeRight` tvar 1
      run (runError $ lookupValueByName $ name "z") `shouldBeRight` tvar 1
      run (runError $ lookupValueByName $ name "v") `shouldBeRight` tvar 0

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

  describe "display" $
    it "displays something" $ do
      display Base                                   `shouldBe` "*"
      display (KFun Base Base)                       `shouldBe` "* -> *"
      display (KFun Base Base `KFun` Base)           `shouldBe` "(* -> *) -> *"
      display (Base `KFun` KFun Base Base)           `shouldBe` "* -> * -> *"
      display (KFun Base Base `KFun` KFun Base Base) `shouldBe` "(* -> *) -> * -> *"

      display ((KFun Base Base `KFun` Base) `KFun` KFun Base Base)                       `shouldBe` "((* -> *) -> *) -> * -> *"
      display ((KFun Base Base `KFun` KFun Base Base) `KFun` KFun Base (KFun Base Base)) `shouldBe` "((* -> *) -> * -> *) -> * -> * -> *"

      display (BaseType Int)                                           `shouldBe` "int"
      display (tvar 3)                                                 `shouldBe` "v[3]"
      display (TFun (tvar 2) (tvar 44))                                `shouldBe` "v[2] -> v[44]"
      display (TFun (tvar 2) (tvar 44) `TFun` tvar 3)                  `shouldBe` "(v[2] -> v[44]) -> v[3]"
      display (tvar 44 `TFun` TFun (tvar 87) (tvar 3))                 `shouldBe` "v[44] -> v[87] -> v[3]"
      display (TFun (tvar 2) (tvar 44) `TFun` TFun (tvar 87) (tvar 3)) `shouldBe` "(v[2] -> v[44]) -> v[87] -> v[3]"

      display (Forall Base $ tvar 0)                                  `shouldBe` "∀*. v[0]"
      display (TApp (tvar 0) (tvar 2))                                `shouldBe` "v[0] v[2]"
      display (TApp (tvar 0) (tvar 2) `TApp` tvar 1)                  `shouldBe` "v[0] v[2] v[1]"
      display (tvar 2 `TApp` TApp (tvar 99) (tvar 1))                 `shouldBe` "v[2] (v[99] v[1])"
      display (TApp (tvar 0) (tvar 2) `TApp` TApp (tvar 99) (tvar 1)) `shouldBe` "v[0] v[2] (v[99] v[1])"

      display (TApp (tvar 0) (tvar 2) `TFun` TApp (tvar 99) (tvar 1)) `shouldBe` "v[0] v[2] -> v[99] v[1]"
      display (TFun (tvar 0) (tvar 2) `TApp` TFun (tvar 99) (tvar 1)) `shouldBe` "(v[0] -> v[2]) (v[99] -> v[1])"

      display (Some Base (tvar 2) `TApp` TAbs (KFun Base Base) (TApp (tvar 33) $ BaseType Bool)) `shouldBe` "(∃*. v[2]) (λ* -> *. v[33] bool)"

      display (record mempty :: Record Type)                                             `shouldBe` "{}"
      display (record [(label "a", BaseType Char)])                                      `shouldBe` "{a: char}"
      display (record [(label "a", BaseType Char), (label "abc", tvar 3 `TFun` tvar 1)]) `shouldBe` "{a: char, abc: v[3] -> v[1]}"

      let ?nctx = nameContext
      displayWithName (tvar 0)                                         `shouldBe` "v[0]"
      displayWithName (Forall Base $ tvar 0)                           `shouldBe` "∀t0 : *. t0"
      displayWithName (Forall Base $ Forall Base $ tvar 0)             `shouldBe` "∀t0 : *. ∀t1 : *. t1"
      displayWithName (Forall Base $ Forall Base $ tvar 1)             `shouldBe` "∀t0 : *. ∀t1 : *. t0"
      displayWithName (Forall Base $ Forall Base $ tvar 2)             `shouldBe` "∀t0 : *. ∀t1 : *. v[2]"
      displayWithName (Forall Base (tvar 0) `TFun` Some Base (tvar 1)) `shouldBe` "(∀t0 : *. t0) -> ∃t0 : *. v[1]"
