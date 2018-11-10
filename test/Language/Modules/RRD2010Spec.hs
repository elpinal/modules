module Language.Modules.RRD2010Spec where

import Test.Hspec

import Data.Coerce
import qualified Data.Map.Lazy as Map

import Language.Modules.RRD2010
import qualified Language.Modules.RRD2010.Internal as I

runElaborate :: Elaboration a => a -> Either TypeError (Output a)
runElaborate = runEnv (I.env :: I.Env SemanticSig) . elaborate

label :: String -> I.Label
label = I.Label . coerce

spec :: Spec
spec = do
  describe "elaborate" $
    it "elaborates external constructs into internal constructs" $ do
      let ty1 = I.TRecord $ coerce $ Map.singleton (label "x") $ I.TRecord $ coerce $ Map.singleton I.Val I.Int
      let t11 = I.Abs (I.TRecord $ coerce $ Map.singleton I.Val I.Int) $
                I.TmRecord $ coerce (Map.singleton (label "x") $ I.Proj (var 1) $ label "x")
      let t1 = I.Abs ty1 $ I.App t11 $ I.Proj (var 0) $ label "x"
      let t2 = I.TmRecord $ coerce $ Map.singleton (label "x") $ I.TmRecord (coerce $ Map.singleton I.Val $ I.IntLit 1)

      runElaborate (Bindings [Val (Ident $ coerce "x") $ IntLit 1]) `shouldBe` Right (I.App t1 t2, existential $ StructureSig $ Map.singleton (label "x") $ AtomicTerm I.Int)
