{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Modules.Ros2018Spec where

import Test.Hspec

import Control.Monad.Freer

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Position
import Language.Modules.Ros2018.Internal (emptyEnv, var, variable, label, record, Literal(..), BaseType(..), Failure(..))
import qualified Language.Modules.Ros2018.Internal as I

shouldBeRight :: (HasCallStack, Eq a, Show a) => Either Failure a -> a -> Expectation
shouldBeRight (Left (Failure err _ f)) _ = expectationFailure $ "error: " ++ f err
shouldBeRight (Right x) expected         = x `shouldBe` expected

dummyP :: a -> Positional a
dummyP = positional dummyPos

spec :: Spec
spec = do
  describe "elaboration" $
    it "elaborates external objects into internal objects" $ do
      let ?env = emptyEnv :: Env
      run (elaborate $ dummyP $ LBool True) `shouldBe` Bool
      run (elaborate $ dummyP $ LInt 778)   `shouldBe` Int
      run (elaborate $ dummyP $ LChar 'o')  `shouldBe` Char

      runElaborate (elaborate $ dummyP $ Lit $ LChar 'o') `shouldBeRight` return (I.Lit (LChar 'o'), fromBody $ BaseType Char, Pure)

      runElaborate (elaborate $ dummyP $ Val (ident "x") $ dummyP $ Lit $ LChar 'o') `shouldBeRight` return
        ( I.Let [I.Lit (LChar 'o')] $ I.TmRecord $ I.record [(label "x", var 0)]
        , fromBody $ record [(label "x", BaseType Char)]
        , Pure
        )

  describe "lookupInsts" $
    it "look up instantiations" $ do
      lookupInsts [] (BaseType Int) (BaseType Int) `shouldBe` []

      lookupInsts [variable 0] (BaseType Int) (SemanticPath $ fromVariable $ variable 0) `shouldBe` [I.BaseType Int]
      lookupInsts [variable 0]
        (Function $ fromBody $ Fun (BaseType Bool) Pure $ fromBody $ BaseType Char)
        (Function $ fromBody $ Fun (BaseType Bool) Pure $ fromBody $ SemanticPath $ fromVariable $ variable 0) `shouldBe` [I.BaseType Char]
