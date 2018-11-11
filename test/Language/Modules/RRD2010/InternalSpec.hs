module Language.Modules.RRD2010.InternalSpec where

import Test.Hspec

import qualified Data.Map.Lazy as Map

import Language.Modules.RRD2010.Internal

var :: Int -> Term
var = Var . Variable

tvar :: Int -> Type
tvar = TVar . Variable

label :: String -> Label
label = Label . Name

tRecord :: [(Label, Type)] -> Type
tRecord = TRecord . Record . Map.fromList

spec :: Spec
spec = do
  describe "typeOf" $ do
    it "typechecks terms" $ do
      runTypeOf env (IntLit 123)                                                `shouldBe` return Int
      runTypeOf env (Abs Int $ var 0)                                           `shouldBe` return (TFun Int Int)
      runTypeOf env (Abs Int $ App (Abs Int $ IntLit 2) $ var 0)                `shouldBe` return (TFun Int Int)
      runTypeOf env (TmRecord $ Record $ Map.singleton (label "x") $ IntLit 33) `shouldBe` return (TRecord $ Record $ Map.singleton (label "x") Int)
      runTypeOf env (Poly Mono $ Abs (tvar 0) $ var 0)                          `shouldBe` return (Forall Mono $ tvar 0 `TFun` tvar 0)
      runTypeOf env (Inst (Poly Mono $ Abs (tvar 0) $ var 0) Int)               `shouldBe` return (TFun Int Int)
      runTypeOf env (Pack Int (IntLit 3) $ Some Mono $ tvar 0)                  `shouldBe` return (Some Mono $ tvar 0)
      runTypeOf env (Abs (Some Mono $ tvar 0) $ Unpack (var 0) $ IntLit 3)      `shouldBe` return (TFun (Some Mono $ tvar 0) Int)

      let ty = Some Mono $ tRecord [(label "x", tvar 0), (label "y", tvar 0 `TFun` Int)]
      runTypeOf env (Abs ty $ Unpack (var 0) $ (var 0 `Proj` label "y") `App` (var 0 `Proj` label "x")) `shouldBe` return (TFun ty Int)

    it "returns an error if given an ill-typed term" $ do
      runTypeOf env (Abs Int $ App (var 0) $ var 0)                        `shouldBe` Left (NotFunction Int)
      runTypeOf env (Pack Int (IntLit 3) $ Some (KFun Mono Mono) $ tvar 0) `shouldBe` Left (NotMono $ KFun Mono Mono)
      runTypeOf env (Abs (Some Mono $ tvar 0) $ Unpack (var 0) $ var 0)    `shouldBe` Left (NoSuchTypeVariable $ Variable 0)
