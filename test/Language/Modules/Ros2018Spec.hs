{-# LANGUAGE ImplicitParams #-}

module Language.Modules.Ros2018Spec where

import Test.Hspec

import Control.Monad.Freer
import Control.Monad.Freer.Error

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Position
import Language.Modules.Ros2018.Internal (emptyEnv, var, label, record, Literal(..), BaseType(..), Failure(..))
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

      run (runError $ elaborate $ dummyP $ Lit $ LChar 'o')  `shouldBeRight` (I.Lit (LChar 'o'), fromBody $ BaseType Char, Pure)

      run (runError $ elaborate $ dummyP $ Val (ident "x") $ dummyP $ Lit $ LChar 'o') `shouldBeRight`
        ( I.Unpack Nothing (I.Lit (LChar 'o')) 0 $ I.TmRecord $ I.record [(label "x", var 0)]
        , fromBody $ Structure $ record [(label "x", BaseType Char)]
        , Pure
        )
