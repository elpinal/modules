{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Modules.Ros2018Spec where

import Test.Hspec

import Control.Monad.Freer

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Position
import Language.Modules.Ros2018.Impl
import Language.Modules.Ros2018.Internal (emptyEnv, var, tvar, variable, label, record, insertType, Literal(..), BaseType(..), Failure(..))
import qualified Language.Modules.Ros2018.Internal as I

shouldBeRight :: (HasCallStack, Eq a, Show a) => Either Failure a -> a -> Expectation
shouldBeRight (Left (Failure err _ f)) _ = expectationFailure $ "error: " ++ f err
shouldBeRight (Right x) expected         = x `shouldBe` expected

dummyP :: a -> Positional a
dummyP = positional dummyPos

runElaborate = runM_ 0

spec :: Spec
spec = do
  describe "elaboration" $
    it "elaborates external objects into internal objects" $ do
      let ?env = emptyEnv :: Env
      run (elaborate $ dummyP $ LBool True) `shouldBe` Bool
      run (elaborate $ dummyP $ LInt 778)   `shouldBe` Int
      run (elaborate $ dummyP $ LChar 'o')  `shouldBe` Char

      runElaborate (elaborate $ dummyP $ Lit $ LChar 'o') `shouldBeRight` return (I.Lit (LChar 'o'), fromBody $ BaseType Char, Pure)

      runElaborate (elaborate $ dummyP $ val (ident "x") $ dummyP $ Lit $ LChar 'o') `shouldBeRight` return
        ( I.Let [I.Lit (LChar 'o')] $ I.TmRecord $ I.record [(label "x", var 0)]
        , fromBody $ record [(label "x", BaseType Char)]
        , Pure
        )

      runElaborate (elaborate $ dummyP $ arrowP Nothing (dummyP TypeType) (dummyP TypeType)) `shouldBeRight` return (quantify [dummyP $ I.KFun I.Base I.Base] $ Function $ quantify [dummyP I.Base] $ Fun (AbstractType $ fromBody $ SemanticPath $ fromVariable $ variable 0) Pure $ fromBody $ AbstractType $ fromBody $ SemanticPath $ Path (variable 1) [SemanticPath $ fromVariable $ variable 0])

  describe "lookupInsts" $
    it "look up instantiations" $ do
      lookupInsts [] (BaseType Int) (BaseType Int) `shouldBe` return []

      lookupInsts [variable 0] (AbstractType $ fromBody $ BaseType Int) (AbstractType $ fromBody $ SemanticPath $ fromVariable $ variable 0) `shouldBe` return [parameterized $ BaseType Int]
      lookupInsts [variable 0]
        (Function $ fromBody $ Fun (BaseType Bool) Pure $ fromBody $ AbstractType $ fromBody $ BaseType Char)
        (Function $ fromBody $ Fun (BaseType Bool) Pure $ fromBody $ AbstractType $ fromBody $ SemanticPath $ fromVariable $ variable 0) `shouldBe` return [parameterized $ BaseType Char]

  describe "match" $
    it "performs signature matching" $ do
      let right x = return x
      let runEl x = either (error "") id $ runElaborate x
      let ?env = emptyEnv :: Env

      (runEl $ match' (BaseType Int) (fromBody $ BaseType Int)) `shouldBe` right (I.Abs (I.BaseType Int) $ var 0, [])
      (runEl $ match' (AbstractType $ fromBody $ BaseType Int) (quantify [dummyP I.Base] $ AbstractType $ fromBody $ SemanticPath $ fromVariable $ variable 0)) `shouldBe` right (I.Abs (I.BaseType Int `I.TFun` I.TRecord []) $ I.Abs (I.BaseType Int) $ I.TmRecord [], [I.BaseType Int])

      let ?env = insertType $ dummyP I.Base
      (runEl $ match' (AbstractType $ fromBody $ SemanticPath $ fromVariable $ variable 0) (quantify [dummyP I.Base] $ AbstractType $ fromBody $ SemanticPath $ fromVariable $ variable 0)) `shouldBe` right (I.Abs (I.TFun (tvar 0) $ I.TRecord []) $ I.Abs (tvar 0) $ I.TmRecord [], [I.tvar 0])

  describe "applySmall" $
    it "performs parallel substitution" $ do
      applySmall [(variable 0, Parameterized [I.Base] $ SemanticPath $ fromVariable $ variable 0)] (SemanticPath $ Path (variable 0) [BaseType Int]) `shouldBe` BaseType Int

  describe "applyPath" $
    it "performs parallel substitution on paths" $ do
      let path n xs = Path (variable n) xs
      applyPath [] (path 0 [])                                                                     `shouldBe` SemanticPath (path 0 [])
      applyPath [(variable 0, parameterized $ BaseType Char)] (path 0 [])                          `shouldBe` BaseType Char
      applyPath [(variable 0, parameterized $ BaseType Char)] (path 1 [])                          `shouldBe` SemanticPath (path 1 [])
      applyPath [(variable 0, parameterized $ SemanticPath $ path 10 [])] (path 0 [])              `shouldBe` SemanticPath (path 10 [])
      applyPath [(variable 0, parameterized $ SemanticPath $ path 12 [BaseType Bool])] (path 0 []) `shouldBe` SemanticPath (path 12 [BaseType Bool])

      applyPath [(variable 0, Parameterized [I.Base] $ SemanticPath $ path 0 [])] (path 0 [BaseType Int])               `shouldBe` BaseType Int
      applyPath [(variable 0, Parameterized [I.Base] $ SemanticPath $ path 12 [BaseType Bool])] (path 0 [BaseType Int]) `shouldBe` SemanticPath (path 11 [BaseType Bool])

      -- Currently, the mismatch of kinds is ignored.
      -- It's unclear whether this is correct.
      applyPath [(variable 0, Parameterized [I.KFun I.Base I.Base] $ SemanticPath $ path 0 [])] (path 0 [BaseType Int]) `shouldBe` BaseType Int

      applyPath [(variable 0, parameterized $ SemanticPath $ path 0 [])] (path 0 [BaseType Int]) `shouldBe` SemanticPath (path 0 [BaseType Int])
