module Language.Modules.RRD2010Spec where

import Test.Hspec

import Data.Bifunctor
import Data.Coerce
import qualified Data.Map.Lazy as Map

import Language.Modules.RRD2010
import qualified Language.Modules.RRD2010.Internal as I

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
      sound (Bindings [Module (ident "x") $ Bindings [Val (ident "x1") $ IntLit 2], Module (ident "y") $ ident "x" :> sig]) `shouldBe` return True
