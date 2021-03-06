{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Modules.Ros2018.InternalSpec where

import Test.Hspec

import Language.Modules.Ros2018.Internal.ForTest

import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Internal
import Language.Modules.Ros2018.Internal.Impl
import Language.Modules.Ros2018.Position
import Language.Modules.Ros2018.Shift

shouldBeRight :: (HasCallStack, Eq a, Show a) => Either Failure a -> a -> Expectation
shouldBeRight (Left (Failure err _ f)) _ = expectationFailure $ "error: " ++ f err
shouldBeRight (Right x) expected         = x `shouldBe` expected

mkShouldBeError ''EnvError 'EvidEnv
mkShouldBeError ''TypeEquivError 'EvidTypeEquiv
mkShouldBeError ''KindError 'EvidKind
mkShouldBeError ''TypeError 'EvidType

gvar :: Int -> Term
gvar = GVar . generated

let1 :: Term -> Term -> Term
let1 t1 = Let [t1]

int :: Type
int = BaseType Int

char :: Type
char = BaseType Char

spec :: Spec
spec = do
  describe "lookupType" $
    it "look up information about a type variable in an environment" $ do
      let ?env = emptyEnv :: Env [] ()
      (runFailure $ lookupType $ variable 0) `shouldBeEnvError` UnboundTypeVariable (variable 0)

      let ?env = insertType [Base]
      (runFailure $ lookupType $ variable 0) `shouldBeRight` [Base]

      (runFailure $ lookupType $ variable 1) `shouldBeEnvError` UnboundTypeVariable (variable 1)

  describe "insertType" $
    it "inserts a new type variable into an environment" $ do
      let vv = Var . variable

      let ?env = emptyEnv :: Env [] Type
      let ?env = insertType [Base]
      let ?env = insertValue (name "x") $ BaseType Char

      (runFailure $ lookupValueByName dummyPos $ name "x") `shouldBeRight` (BaseType Char, vv 0)

      let ?env = insertValue (name "y") $ tvar 0
      (runFailure $ lookupValueByName dummyPos $ name "y") `shouldBeRight` (tvar 0, vv 0)

      let ?env = insertValue (name "z") $ tvar 0
      (runFailure $ lookupValueByName dummyPos $ name "y") `shouldBeRight` (tvar 0, vv 1)
      (runFailure $ lookupValueByName dummyPos $ name "z") `shouldBeRight` (tvar 0, vv 0)

      let ?env = insertType [Base]
      (runFailure $ lookupValueByName dummyPos $ name "y") `shouldBeRight` (tvar 1, vv 1)
      (runFailure $ lookupValueByName dummyPos $ name "z") `shouldBeRight` (tvar 1, vv 0)

      let ?env = insertValue (name "v") $ tvar 0
      (runFailure $ lookupValueByName dummyPos $ name "x") `shouldBeRight` (BaseType Char, vv 3)
      (runFailure $ lookupValueByName dummyPos $ name "y") `shouldBeRight` (tvar 1, vv 2)
      (runFailure $ lookupValueByName dummyPos $ name "z") `shouldBeRight` (tvar 1, vv 1)
      (runFailure $ lookupValueByName dummyPos $ name "v") `shouldBeRight` (tvar 0, vv 0)

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

      display (Forall Base $ tvar 0)                                  `shouldBe` "∀t0 : *. t0"
      display (TApp (tvar 0) (tvar 2))                                `shouldBe` "v[0] v[2]"
      display (TApp (tvar 0) (tvar 2) `TApp` tvar 1)                  `shouldBe` "v[0] v[2] v[1]"
      display (tvar 2 `TApp` TApp (tvar 99) (tvar 1))                 `shouldBe` "v[2] (v[99] v[1])"
      display (TApp (tvar 0) (tvar 2) `TApp` TApp (tvar 99) (tvar 1)) `shouldBe` "v[0] v[2] (v[99] v[1])"

      display (TApp (tvar 0) (tvar 2) `TFun` TApp (tvar 99) (tvar 1)) `shouldBe` "v[0] v[2] -> v[99] v[1]"
      display (TFun (tvar 0) (tvar 2) `TApp` TFun (tvar 99) (tvar 1)) `shouldBe` "(v[0] -> v[2]) (v[99] -> v[1])"

      display (Some Base (tvar 2) `TApp` TAbs (KFun Base Base) (TApp (tvar 33) $ BaseType Bool)) `shouldBe` "(∃t0 : *. v[2]) (λt0 : * -> *. v[33] bool)"

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

      displayWithName (Unpack Nothing (var 2) 0 $ var 0) `shouldBe` "unpack [v0] = v[2] in v0"
      displayWithName (Unpack Nothing (var 2) 0 $ var 1) `shouldBe` "unpack [v0] = v[2] in v[1]"

      displayWithName (Unpack Nothing (var 2) 0 $ Abs (tvar 0) $ var 0)                 `shouldBe` "unpack [v0] = v[2] in λv1 : v[0]. v1"
      displayWithName (Unpack Nothing (var 2) 1 $ Abs (tvar 0) $ var 0)                 `shouldBe` "unpack [v0, t1] = v[2] in λv2 : t1. v2"
      displayWithName (Unpack Nothing (var 2) 2 $ Abs (tvar 0) $ var 0)                 `shouldBe` "unpack [v0, t1..t2] = v[2] in λv3 : t2. v3"
      displayWithName (Unpack Nothing (Abs (tvar 0) $ var 0) 30 $ Abs (tvar 1) $ var 0) `shouldBe` "unpack [v0, t1..t30] = λv0 : v[0]. v0 in λv31 : t29. v31"

      displayWithName (Unpack (Just $ generated 3) (var 2) 1 $ Abs (tvar 0) $ var 0) `shouldBe` "unpack [g3, t0] = v[2] in λv1 : t0. v1"
      displayWithName (Unpack (Just $ generated 3) (var 2) 1 $ var 0)                `shouldBe` "unpack [g3, t0] = v[2] in v[0]"
      displayWithName (Unpack (Just $ generated 3) (var 2) 1 $ gvar 4)               `shouldBe` "unpack [g3, t0] = v[2] in g4"
      displayWithName (Unpack (Just $ generated 3) (var 2) 1 $ gvar 3)               `shouldBe` "unpack [g3, t0] = v[2] in g3"

  describe "substTop" $
    it "beta reduction" $ do
      substTop char int `shouldBe` int

  describe "apply" $
    it "performs parallel substitution" $ do
      apply [] int                                                    `shouldBe` int
      apply [] (tvar 0)                                               `shouldBe` tvar 0
      apply [(variable 0, int)] (tvar 0)                              `shouldBe` int
      apply [(variable 1, int)] (tvar 0)                              `shouldBe` tvar 0
      apply [(variable 0, int)] (tvar 1)                              `shouldBe` tvar 1
      apply [(variable 0, tvar 0)] (tvar 0)                           `shouldBe` tvar 0
      apply [(variable 0, tvar 20)] (tvar 20)                         `shouldBe` tvar 20
      apply [(variable 0, tvar 20), (variable 20, tvar 40)] (tvar 20) `shouldBe` tvar 40
      apply [(variable 0, tvar 20), (variable 20, tvar 40)] (tvar 0)  `shouldBe` tvar 20
      apply [(variable 0, tvar 20), (variable 20, tvar 40)] (tvar 39) `shouldBe` tvar 39

      apply [(variable 0, char)] (Forall Base int)        `shouldBe` Forall Base int
      apply [(variable 0, char)] (Forall Base $ tvar 0)   `shouldBe` Forall Base (tvar 0)
      apply [(variable 1, char)] (Forall Base $ tvar 0)   `shouldBe` Forall Base (tvar 0)
      apply [(variable 1, char)] (Forall Base $ tvar 1)   `shouldBe` Forall Base (tvar 1)
      apply [(variable 0, char)] (Forall Base $ tvar 1)   `shouldBe` Forall Base char
      apply [(variable 0, tvar 0)] (Forall Base $ tvar 1) `shouldBe` Forall Base (tvar 1)
      apply [(variable 0, tvar 1)] (Forall Base $ tvar 1) `shouldBe` Forall Base (tvar 2)
      apply [(variable 0, tvar 2)] (Forall Base $ tvar 1) `shouldBe` Forall Base (tvar 3)

      apply [(variable 0, Some Base $ tvar 0)] (Forall Base $ tvar 1) `shouldBe` Forall Base (Some Base $ tvar 0)

  describe "reduce" $
    it "reduces a type to weak-head normal form" $ do
      reduce (BaseType Int)                       `shouldBe` BaseType Int
      reduce (tvar 0)                             `shouldBe` tvar 0
      reduce (TAbs Base $ tvar 0)                 `shouldBe` TAbs Base (tvar 0)
      reduce (TAbs Base (tvar 0) `TApp` tvar 333) `shouldBe` tvar 333
      reduce (TAbs Base int `TApp` char)          `shouldBe` int
      reduce (TAbs Base (tvar 0) `TApp` int)      `shouldBe` int

      let ty = TAbs (KFun Base Base) (TApp (tvar 0) int) `TApp` TAbs Base (tvar 0)
      reduce ty `shouldBe` int

  describe "equal" $
    it "tests type equivalence" $ do
      let ?env = emptyEnv :: Env Id Type
      (runFailure $ equal (BaseType Int) (BaseType Int) Base)  `shouldBeRight` ()
      (runFailure $ equal (BaseType Int) (BaseType Bool) Base) `shouldBeTypeEquivError` StructurallyInequivalent (BaseType Int) (BaseType Bool)

      (runFailure $ equal (TAbs Base int `TApp` char) int Base) `shouldBeRight` ()

      let ty = TAbs (KFun Base Base) (TApp (tvar 0) int) `TApp` TAbs Base (tvar 0)
      (runFailure $ equal ty int Base) `shouldBeRight` ()

      let ?env = insertType $ Id $ KFun Base Base
      (runFailure $ equal (tvar 0) (tvar 0) $ KFun Base Base) `shouldBeRight` ()

      (runFailure $ equal (TAbs Base (tvar 1) `TApp` tvar 0) (tvar 0) $ KFun Base Base) `shouldBeRight` ()

  describe "kindOf" $
    it "obtains the kind of a type" $ do
      let ?env = emptyEnv :: Env Id Type
      (runFailure $ kindOf $ BaseType Char)        `shouldBeRight` Base
      (runFailure $ kindOf $ Forall Base $ tvar 0) `shouldBeRight` Base
      (runFailure $ kindOf $ TAbs Base $ tvar 0)   `shouldBeRight` KFun Base Base

      (runFailure $ kindOf $ tvar 0) `shouldBeEnvError` UnboundTypeVariable (variable 0)

      (runFailure $ kindOf $ Forall (KFun Base Base) $ BaseType Char) `shouldBeRight` Base
      (runFailure $ kindOf $ Forall (KFun Base Base) $ tvar 0)        `shouldBeKindError` NotBase (KFun Base Base)

      (runFailure $ kindOf $ TApp (TAbs Base $ tvar 0) $ BaseType Int)                          `shouldBeRight` Base
      (runFailure $ kindOf $ TApp (TAbs (KFun Base Base) $ tvar 0) $ TAbs Base $ BaseType Bool) `shouldBeRight` KFun Base Base
      (runFailure $ kindOf $ TApp (TAbs (KFun Base Base) $ BaseType Char) $ TAbs Base $ tvar 0) `shouldBeRight` Base

      (runFailure $ kindOf $ TApp (TAbs (KFun Base Base) $ BaseType Char) $ BaseType Int) `shouldBeKindError` KindMismatch_ (KFun Base Base) Base
      (runFailure $ kindOf $ TApp (TAbs Base $ BaseType Char) $ TAbs Base $ tvar 0)       `shouldBeKindError` KindMismatch_ Base (KFun Base Base)
      (runFailure $ kindOf $ TApp (BaseType Int) $ TAbs Base $ tvar 0)                    `shouldBeKindError` NotFunctionKind Base

  describe "typeOf" $
    it "obtains the type of a term" $ do
      let ?env = emptyEnv :: Env Id Type
      (runFailure $ typeOf $ LChar 'w') `shouldBeRight` BaseType Char
      (runFailure $ typeOf $ var 0)     `shouldBeEnvError` UnboundVariable (variable 0)
      (runFailure $ typeOf $ gvar 0)    `shouldBeEnvError` UnboundGeneratedVariable (generated 0)

      (runFailure $ typeOf $ Abs int $ var 0)                 `shouldBeRight` TFun int int
      (runFailure $ typeOf $ Abs int $ gvar 0)                `shouldBeEnvError` UnboundGeneratedVariable (generated 0)
      (runFailure $ typeOf $ Abs (tvar 0) $ Lit $ LBool True) `shouldBeEnvError` UnboundTypeVariable (variable 0)
      (runFailure $ typeOf $ Abs (tvar 0) $ var 0)            `shouldBeEnvError` UnboundTypeVariable (variable 0)

      (runFailure $ typeOf $ Abs (TAbs Base $ tvar 0) $ var 0)             `shouldBeKindError` NotBase (KFun Base Base)
      (runFailure $ typeOf $ Abs (TAbs Base $ tvar 0) $ Lit $ LBool False) `shouldBeKindError` NotBase (KFun Base Base)

      (runFailure $ typeOf $ App (Lit $ LInt 3) $ Lit $ LChar 'x')    `shouldBeTypeError` NotFunction int
      (runFailure $ typeOf $ App (Abs int $ var 0) $ Lit $ LChar 'x') `shouldBeTypeEquivError` StructurallyInequivalent int char
      (runFailure $ typeOf $ App (Abs int $ var 0) $ Lit $ LInt 48)   `shouldBeRight` int

      let ty = TAbs Base int `TApp` char
      (runFailure $ typeOf $ Abs (TAbs Base int `TApp` char) $ var 0)   `shouldBeRight` TFun ty ty
      (runFailure $ whTypeOf $ Abs (TAbs Base int `TApp` char) $ var 0) `shouldBeRight` TFun ty ty

      let ty = TAbs (KFun Base Base) (TApp (tvar 0) int) `TApp` TAbs Base (tvar 0)
      (runFailure $ typeOf $ Abs ty $ var 0)   `shouldBeRight` TFun ty ty
      (runFailure $ whTypeOf $ Abs ty $ var 0) `shouldBeRight` TFun ty ty

      (runFailure $ whTypeOf $ App (Abs (TAbs Base (tvar 0) `TApp` int) $ var 0) $ Lit $ LInt 48)                                       `shouldBeRight` int
      (runFailure $ typeOf $ App (Abs (TAbs Base (tvar 1) `TApp` int) $ var 0) $ Lit $ LInt 48)                                         `shouldBeEnvError` UnboundTypeVariable (variable 1)
      (runFailure $ whTypeOf $ App (Abs (TAbs Base int `TApp` int) $ var 0) $ Lit $ LInt 48)                                            `shouldBeRight` int
      (runFailure $ whTypeOf $ App (Abs (TAbs Base int `TApp` char) $ var 0) $ Lit $ LInt 48)                                           `shouldBeRight` int
      (runFailure $ typeOf $ App (Abs (TAbs (KFun Base Base) int `TApp` char) $ var 0) $ Lit $ LInt 48)                                 `shouldBeKindError` KindMismatch_ (KFun Base Base) Base
      (runFailure $ whTypeOf $ App (Abs (TAbs (KFun Base Base) int `TApp` TAbs Base (tvar 0)) $ var 0) $ Lit $ LInt 48)                 `shouldBeRight` int
      (runFailure $ whTypeOf $ App (Abs (TAbs (KFun Base Base) (TApp (tvar 0) int) `TApp` TAbs Base (tvar 0)) $ var 0) $ Lit $ LInt 48) `shouldBeRight` int

      (runFailure $ whTypeOf $ TmRecord $ record [])                          `shouldBeRight` TRecord (record [])
      (runFailure $ whTypeOf $ TmRecord $ record [(label "a", Lit $ LInt 3)]) `shouldBeRight` TRecord (record [(label "a", int)])

      (runFailure $ whTypeOf $ Proj (TmRecord $ record [(label "a", Lit $ LInt 3)]) $ label "a") `shouldBeRight` int
      (runFailure $ whTypeOf $ Proj (TmRecord $ record [(label "a", Lit $ LInt 3)]) $ label "b") `shouldBeTypeError` MissingLabel (label "b") (record [(label "a", int)])

      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [] [] char) `shouldBeRight` char
      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [] [] int)  `shouldBeTypeEquivError` StructurallyInequivalent int char

      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [char] [Base] char)                    `shouldBeRight` Some Base char
      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [char] [Base] $ tvar 0)                `shouldBeRight` Some Base (tvar 0)
      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [int] [Base] $ tvar 0)                 `shouldBeTypeEquivError` StructurallyInequivalent int char
      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [char] [KFun Base Base] $ tvar 0)      `shouldBeKindError` KindMismatch_ (KFun Base Base) Base
      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [TAbs Base int] [KFun Base Base] char) `shouldBeRight` Some (KFun Base Base) char
      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [char, int] [Base, Base] $ tvar 0)     `shouldBeRight` Some Base (Some Base $ tvar 0)
      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [int, char] [Base, Base] $ tvar 1)     `shouldBeRight` Some Base (Some Base $ tvar 1)
      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [char, int] [Base, Base] $ tvar 1)     `shouldBeTypeEquivError` StructurallyInequivalent int char
      (runFailure $ whTypeOf $ Pack (Lit $ LChar 'v') [int, char] [Base, Base] $ tvar 0)     `shouldBeTypeEquivError` StructurallyInequivalent int char

      (runFailure $ whTypeOf $ Abs (Some Base int) $ Unpack Nothing (var 0) 0 $ var 0)      `shouldBeRight` TFun (Some Base int) (Some Base int)
      (runFailure $ whTypeOf $ Abs (Some Base int) $ Unpack Nothing (var 0) 1 $ var 0)      `shouldBeRight` TFun (Some Base int) int
      (runFailure $ whTypeOf $ Abs (Some Base $ tvar 0) $ Unpack Nothing (var 0) 0 $ var 0) `shouldBeRight` TFun (Some Base $ tvar 0) (Some Base $ tvar 0)
      (runFailure $ whTypeOf $ Abs (Some Base $ tvar 0) $ Unpack Nothing (var 0) 1 $ var 0) `shouldBeEnvError` UnboundTypeVariable (variable (-1))

      (runFailure $ whTypeOf $ Abs (Some Base $ tvar 0) $ Unpack Nothing (var 0) 2 $ var 0)                                `shouldBeTypeError` NotSome (tvar 0)
      (runFailure $ whTypeOf $ Abs (Some Base $ Some (KFun Base Base) $ tvar 0) $ Unpack Nothing (var 0) 2 $ var 0)        `shouldBeKindError` NotBase (KFun Base Base)
      (runFailure $ whTypeOf $ Abs (Some Base $ Some (KFun Base Base) $ tvar 1) $ Unpack Nothing (var 0) 2 $ var 0)        `shouldBeEnvError` UnboundTypeVariable (variable (-1))
      (runFailure $ whTypeOf $ Abs (Some Base $ Some (KFun Base Base) $ tvar 1) $ Unpack Nothing (var 0) 2 $ Lit $ LInt 7) `shouldBeRight` TFun (Some Base $ Some (KFun Base Base) $ tvar 1) int
