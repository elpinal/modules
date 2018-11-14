{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Modules.RRD2014Spec where

import Test.Hspec

import Data.Bifunctor
import Data.Coerce
import Data.Either
import qualified Data.Map.Lazy as Map

import Language.Modules.RRD2014
import qualified Language.Modules.RRD2014.Internal as I

runElaborate :: Elaboration a => a -> Either TypeError (Output a)
runElaborate = runEnv (I.env :: I.Env SemanticSig) . elaborate

label :: String -> I.Label
label = I.Label . coerce

ident :: String -> Ident
ident = Ident . coerce

sound :: Module -> Either TypeError Bool
sound m = runElaborate m >>= \(t, asig) -> bimap (fromProblem . Internal) (== encode asig) $ I.runTypeOf I.env t

spec :: Spec
spec = do
  describe "elaborate" $ do
    it "elaborates external constructs into internal constructs" $ do
      let ty1 = I.TRecord $ coerce $ Map.singleton (label "x") $ I.TRecord $ coerce $ Map.singleton I.Val I.Int
      let t11 = I.Abs (I.TRecord $ coerce $ Map.singleton I.Val I.Int) $
                I.TmRecord $ coerce (Map.singleton (label "x") $ I.Proj (var 1) $ label "x")
      let t1 = I.Abs ty1 $ I.App t11 $ I.Proj (var 0) $ label "x"
      let t2 = I.TmRecord $ coerce $ Map.singleton (label "x") $ I.TmRecord (coerce $ Map.singleton I.Val $ I.IntLit 1)

      runElaborate (Bindings [Val (ident "x") $ IntLit 1]) `shouldBe` Right (I.App t1 t2, existential $ StructureSig $ Map.singleton (label "x") $ AtomicTerm I.Int)

      let sig = Decls [ValDecl (ident "x1") Int]
      let ssig = StructureSig $ Map.fromList
                  [ (label "x", StructureSig (Map.singleton (label "x1") $ AtomicTerm I.Int))
                  , (label "y", StructureSig (Map.singleton (label "x1") $ AtomicTerm I.Int))
                  ]
      snd <$> runElaborate (Bindings [Module (ident "x") $ Bindings [Val (ident "x1") $ IntLit 2], Module (ident "y") $ ident "x" :> sig]) `shouldBe` return (existential ssig)

    it "elaborates external constructs into well-typed internal constructs" $ do
      sound (Bindings [Val (ident "x") $ IntLit 1]) `shouldBe` return True
      sound (Bindings [Type (ident "x") Int])       `shouldBe` return True

      let m = Bindings [Type (ident "x") Int]
      sound (Bindings [Module (ident "x") m]) `shouldBe` return True

      let sig = Decls [ValDecl (ident "x") Int]
      sound (Bindings [Signature (ident "x") sig]) `shouldBe` return True

      sound (Bindings [Include m]) `shouldBe` return True

      sound (Bindings [Val (ident "x") $ IntLit 1, Type (ident "x") Int]) `shouldBe` return True

      let sig = Decls [ValDecl (ident "x1") Int]
      let m = Bindings [Module (ident "x") $ Bindings [Val (ident "x1") $ IntLit 2], Module (ident "y") $ ident "x" :> sig]
      sound m                          `shouldBe` return True
      sound (Projection m $ ident "x") `shouldBe` return True
      sound (Projection m $ ident "y") `shouldBe` return True
      sound (Projection m $ ident "z") `shouldSatisfy` isLeft

      let pr = Projection (Projection m $ ident "x") $ ident "x1"
      sound pr                                                `shouldBe` return True
      sound (Bindings [Val (ident "a") $ PathExpr $ Path pr]) `shouldBe` return True

      let sig = Decls [AbsTypeDecl (ident "t") Mono, ValDecl (ident "x") $ PathType $ Path $ ModuleIdent $ ident "t"]
      let m = Bindings [Module (ident "m1") $ Bindings [Type (ident "t") Int, Val (ident "x") $ IntLit 4], Module (ident "m2") $ ident "m1" :> sig]
      sound m `shouldBe` return True

      let m = Bindings [Val (ident "x") $ IntLit 1, Val (ident "y") $ IntLit 2, Val (ident "z") $ IntLit 3]
      sound m `shouldBe` return True

      let sig = Decls [ValDecl (ident "x") Int]
      let m = Fun (ident "f") sig $ Bindings [Val (ident "y") $ IntLit 9]
      sound m `shouldBe` return True

      let m = Fun (ident "m") sig $ Bindings [Val (ident "y") $ PathExpr $ Path $ Projection (ModuleIdent $ ident "m") $ ident "x"]
      sound m `shouldBe` return True

      let m1 = Bindings [Val (ident "x") $ IntLit 33]
      let m2 = Fun (ident "m0") sig $ Bindings [Val (ident "y") $ IntLit 66, Val (ident "z") $ PathExpr $ Path $ Projection (ModuleIdent $ ident "m0") $ ident "x"]
      let m = Bindings [Module (ident "m1") m1, Module (ident "m2") m2, Module (ident "app") $ ModuleApp (ident "m2") (ident "m1")]
      sound m `shouldBe` return True

      let sig = Decls [AbsTypeDecl (ident "t1") Mono, AbsTypeDecl (ident "t2") Mono]
      let m1 = Bindings [Type (ident "t1") Int, Type (ident "t2") Int]
      let m = Bindings [Module (ident "m1") m1, Module (ident "m2") $ ident "m1" :> sig]
      sound m `shouldBe` return True

      let wrongSig = Decls [AbsTypeDecl (ident "t1") Mono, ManTypeDecl (ident "t2") Int]
      let m = Bindings [Module (ident "m1") m1, Module (ident "m2") $ ident "m1" :> sig, Module (ident "m3") $ ident "m2" :> wrongSig]
      sound m `shouldBe` Left (fromProblem $ NotEqual (I.TVar $ I.Variable 0) I.Int)

      let sig = Decls [AbsTypeDecl (ident "t1") Mono]
      let m = Bindings [Module (ident "m1") m1, Module (ident "m2") $ ident "m1" :> sig]
      sound m `shouldBe` return True

      let sig1 = Decls [AbsTypeDecl (ident "t") Mono]
      let sig = Where sig1 (Proj (ident "t") []) Int
      let m1 = Bindings [Type (ident "t") Int]
      let m = Bindings [Module (ident "m1") m1, Module (ident "m2") $ ident "m1" :> sig, Type (ident "t") $ PathType $ Path $ Projection (ModuleIdent $ ident "m2") $ ident "t"]
      sound m `shouldBe` return True

    it "supports shadowing of declarations" $ do
      let m = Bindings [Val (ident "x") $ IntLit 1, Val (ident "x") $ IntLit 2, Val (ident "z") $ PathExpr $ Path $ ModuleIdent $ ident "x"]
      sound m `shouldBe` return True

    it "supports first-class (packaged) modules" $ do
      let m1 = Bindings [Val (ident "x") $ IntLit 80]
      let sig = Decls [ValDecl (ident "x") Int]
      let m = Bindings [Val (ident "x") $ Pack m1 sig]
      sound m `shouldBe` return True

      let sig = Decls [ValDecl (ident "y") Int]
      let m = Bindings [Val (ident "x") $ Pack m1 sig]
      sound m `shouldSatisfy` isLeft

      let m1 = Bindings [Type (ident "t") Int]
      let sig = Decls [ManTypeDecl (ident "t") Int]
      let m = Bindings [Val (ident "x") $ Pack m1 sig]
      sound m `shouldBe` return True
