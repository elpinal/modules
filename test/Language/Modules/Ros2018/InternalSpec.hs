{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Modules.Ros2018.InternalSpec where

import Test.Hspec

import Language.Modules.Ros2018.Internal.ForTest

import Control.Monad.Freer
import Control.Monad.Freer.Error

import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Internal
import Language.Modules.Ros2018.Shift

shouldBeRight :: (HasCallStack, Eq a, Show a) => Either Failure a -> a -> Expectation
shouldBeRight (Left (Failure err _ f)) _ = expectationFailure $ "error: " ++ f err
shouldBeRight (Right x) expected         = x `shouldBe` expected

mkShouldBeError ''EnvError 'EvidEnv
mkShouldBeError ''TypeEquivError 'EvidTypeEquiv
mkShouldBeError ''KindError 'EvidKind

var :: Int -> Term
var = Var . variable

gvar :: Int -> Term
gvar = GVar . generated

let1 :: Term -> Term -> Term
let1 t1 = Let [t1]

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

      display (var 0)                                             `shouldBe` "v[0]"
      display (gvar 0)                                            `shouldBe` "g0"
      display (Abs (tvar 0) $ var 0)                              `shouldBe` "λv[0]. v[0]"
      display (App (var 0) (var 1))                               `shouldBe` "v[0] v[1]"
      display (App (var 0) (var 1) `App` App (var 2) (var 99))    `shouldBe` "v[0] v[1] (v[2] v[99])"
      display (Proj (var 0) $ label "a")                          `shouldBe` "v[0].a"
      display (Proj (var 0 `App` var 1) $ label "body")           `shouldBe` "(v[0] v[1]).body"
      display (Proj (Abs (BaseType Bool) $ var 2) $ label "body") `shouldBe` "(λbool. v[2]).body"
      display (Poly Base $ var 0)                                 `shouldBe` "Λ*. v[0]"
      display (Poly (KFun Base Base) $ var 0)                     `shouldBe` "Λ* -> *. v[0]"
      display (Inst (var 0) $ tvar 0)                             `shouldBe` "v[0] [v[0]]"
      display ((Inst (var 0) $ tvar 0) `App` var 28)              `shouldBe` "v[0] [v[0]] v[28]"
      display ((var 3 `App` var 28) `Inst` tvar 38)               `shouldBe` "v[3] v[28] [v[38]]"

      display (Pack (var 0) [] [] $ tvar 1)                                                `shouldBe` "pack [; v[0]] as v[1]"
      display (Pack (var 0) [BaseType Int] [Base] $ tvar 1)                                `shouldBe` "pack [int; v[0]] as ∃*. v[1]"
      display (Pack (var 0) [BaseType Int, BaseType Bool] [KFun Base Base, Base] $ tvar 1) `shouldBe` "pack [bool, int; v[0]] as ∃*. ∃* -> *. v[1]"

      display (Unpack Nothing (var 2) 0 $ var 1)   `shouldBe` "unpack [0] = v[2] in v[1]"
      display (Unpack Nothing (var 2) 1 $ var 7)   `shouldBe` "unpack [1] = v[2] in v[7]"
      display (Unpack Nothing (var 2) 136 $ var 7) `shouldBe` "unpack [136] = v[2] in v[7]"

      display (Unpack (Just $ generated 71) (var 2) 0 $ var 1)     `shouldBe` "unpack [g71, 0] = v[2] in v[1]"
      display (Unpack (Just $ generated 39) (var 2) 1 $ gvar 39)   `shouldBe` "unpack [g39, 1] = v[2] in g39"
      display (Unpack (Just $ generated 42) (var 2) 136 $ gvar 5)  `shouldBe` "unpack [g42, 136] = v[2] in g5"

      display (let1 (var 0) $ var 37)                `shouldBe` "let v[0] in v[37]"
      display (App (let1 (var 0) $ var 37) $ var 3)  `shouldBe` "(let v[0] in v[37]) v[3]"
      display (App (var 84) $ let1 (var 0) $ var 37) `shouldBe` "v[84] (let v[0] in v[37])"

      display (Let [var 0, Poly Base $ var 44] $ var 37) `shouldBe` "let v[0]; Λ*. v[44] in v[37]"

      let ?nctx = nameContext
      displayWithName (tvar 0)                                         `shouldBe` "v[0]"
      displayWithName (Forall Base $ tvar 0)                           `shouldBe` "∀t0 : *. t0"
      displayWithName (Forall Base $ Forall Base $ tvar 0)             `shouldBe` "∀t0 : *. ∀t1 : *. t1"
      displayWithName (Forall Base $ Forall Base $ tvar 1)             `shouldBe` "∀t0 : *. ∀t1 : *. t0"
      displayWithName (Forall Base $ Forall Base $ tvar 2)             `shouldBe` "∀t0 : *. ∀t1 : *. v[2]"
      displayWithName (Forall Base (tvar 0) `TFun` Some Base (tvar 1)) `shouldBe` "(∀t0 : *. t0) -> ∃t0 : *. v[1]"

      displayWithName (var 0)                `shouldBe` "v[0]"
      displayWithName (gvar 0)               `shouldBe` "g0"
      displayWithName (Abs (tvar 0) $ var 0) `shouldBe` "λv0 : v[0]. v0"
      displayWithName (Abs (tvar 0) $ Abs (BaseType Char) $ var 0) `shouldBe` "λv0 : v[0]. λv1 : char. v1"
      displayWithName (Abs (tvar 0) $ Abs (BaseType Char) $ var 1) `shouldBe` "λv0 : v[0]. λv1 : char. v0"

      displayWithName (Poly Base $ var 0)                `shouldBe` "Λt0 : *. v[0]"
      displayWithName (Poly Base $ Abs (tvar 0) $ var 0) `shouldBe` "Λt0 : *. λv1 : t0. v1"

      displayWithName (Pack (var 0) [] [] $ tvar 1)                                                `shouldBe` "pack [; v[0]] as v[1]"
      displayWithName (Pack (var 0) [BaseType Int] [Base] $ tvar 0)                                `shouldBe` "pack [int; v[0]] as ∃t0 : *. t0"
      displayWithName (Pack (var 0) [BaseType Int] [Base] $ tvar 1)                                `shouldBe` "pack [int; v[0]] as ∃t0 : *. v[1]"
      displayWithName (Pack (var 0) [BaseType Int, BaseType Bool] [KFun Base Base, Base] $ tvar 1) `shouldBe` "pack [bool, int; v[0]] as ∃t0 : *. ∃t1 : * -> *. t0"

      displayWithName (Let [] $ var 0)                                  `shouldBe` "let  in v[0]"
      displayWithName (Let [var 0] $ var 0)                             `shouldBe` "let v0 = v[0] in v0"
      displayWithName (Let [var 0, var 1] $ var 0)                      `shouldBe` "let v0 = v[0]; v1 = v[1] in v1"
      displayWithName (Let [var 0, var 1] $ Let [var 0, var 1] $ var 0) `shouldBe` "let v0 = v[0]; v1 = v[1] in let v2 = v1; v3 = v0 in v3"

      displayWithName (Unpack Nothing (var 2) 0 $ var 0) `shouldBe` "unpack [v0, ] = v[2] in v0"
      displayWithName (Unpack Nothing (var 2) 0 $ var 1) `shouldBe` "unpack [v0, ] = v[2] in v[1]"

      displayWithName (Unpack Nothing (var 2) 0 $ Abs (tvar 0) $ var 0)                 `shouldBe` "unpack [v0, ] = v[2] in λv1 : v[0]. v1"
      displayWithName (Unpack Nothing (var 2) 1 $ Abs (tvar 0) $ var 0)                 `shouldBe` "unpack [v0, t1] = v[2] in λv2 : t1. v2"
      displayWithName (Unpack Nothing (var 2) 2 $ Abs (tvar 0) $ var 0)                 `shouldBe` "unpack [v0, t1..t2] = v[2] in λv3 : t2. v3"
      displayWithName (Unpack Nothing (Abs (tvar 0) $ var 0) 30 $ Abs (tvar 1) $ var 0) `shouldBe` "unpack [v0, t1..t30] = λv0 : v[0]. v0 in λv31 : t29. v31"

  describe "reduce" $
    it "reduces a type to weak-head normal form" $ do
      reduce (BaseType Int) `shouldBe` BaseType Int
      reduce (tvar 0) `shouldBe` tvar 0
      reduce (TAbs Base $ tvar 0) `shouldBe` TAbs Base (tvar 0)
      reduce (TAbs Base (tvar 0) `TApp` tvar 333) `shouldBe` tvar 333

  describe "equal" $
    it "tests type equivalence" $ do
      let ?env = emptyEnv :: Env Id Type
      run (runError $ equal (BaseType Int) (BaseType Int) Base)  `shouldBeRight` ()
      run (runError $ equal (BaseType Int) (BaseType Bool) Base) `shouldBeTypeEquivError` StructurallyInequivalent (BaseType Int) (BaseType Bool)

      let ?env = insertType $ Id $ KFun Base Base
      run (runError $ equal (tvar 0) (tvar 0) $ KFun Base Base) `shouldBeRight` ()

      run (runError $ equal (TAbs Base (tvar 1) `TApp` tvar 0) (tvar 0) $ KFun Base Base) `shouldBeRight` ()

  describe "kindOf" $
    it "obtains the kind of a type" $ do
      let ?env = emptyEnv :: Env Id Type
      run (runError $ kindOf $ BaseType Char)        `shouldBeRight` Base
      run (runError $ kindOf $ Forall Base $ tvar 0) `shouldBeRight` Base
      run (runError $ kindOf $ TAbs Base $ tvar 0)   `shouldBeRight` KFun Base Base

      run (runError $ kindOf $ tvar 0) `shouldBeEnvError` UnboundTypeVariable (variable 0)

      run (runError $ kindOf $ Forall (KFun Base Base) $ BaseType Char) `shouldBeRight` Base
      run (runError $ kindOf $ Forall (KFun Base Base) $ tvar 0)        `shouldBeKindError` NotBase (KFun Base Base)

      run (runError $ kindOf $ TApp (TAbs Base $ tvar 0) $ BaseType Int)                          `shouldBeRight` Base
      run (runError $ kindOf $ TApp (TAbs (KFun Base Base) $ tvar 0) $ TAbs Base $ BaseType Bool) `shouldBeRight` KFun Base Base
      run (runError $ kindOf $ TApp (TAbs (KFun Base Base) $ BaseType Char) $ TAbs Base $ tvar 0) `shouldBeRight` Base

      run (runError $ kindOf $ TApp (TAbs (KFun Base Base) $ BaseType Char) $ BaseType Int) `shouldBeKindError` KindMismatch_ (KFun Base Base) Base
      run (runError $ kindOf $ TApp (TAbs Base $ BaseType Char) $ TAbs Base $ tvar 0)       `shouldBeKindError` KindMismatch_ Base (KFun Base Base)
      run (runError $ kindOf $ TApp (BaseType Int) $ TAbs Base $ tvar 0)                    `shouldBeKindError` NotFunctionKind Base

newtype Id a = Id a

instance Annotated Id where
  extract (Id x) = x
  unannotated = Id
