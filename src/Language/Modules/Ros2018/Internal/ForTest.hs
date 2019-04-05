{-# LANGUAGE TemplateHaskell #-}

module Language.Modules.Ros2018.Internal.ForTest where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH hiding (Name)

import Language.Modules.Ros2018.Internal

mkShouldBeError :: TH.Name -> TH.Name -> DecsQ
mkShouldBeError ty evid = do
  let name = mkName $ "shouldBe" ++ nameBase ty
  let fun ty1 ty2 = AppT ArrowT ty1 `AppT` ty2
  let sig = SigD name $
              ForallT
                [PlainTV $ mkName "a"]
                [ConT $ mkName "HasCallStack", AppT (ConT $ mkName "Show") $ VarT $ mkName "a"] $
                (AppT (AppT (ConT $ mkName "Either") $ ConT $ mkName "Failure") $ VarT $ mkName "a") `fun` (ConT ty `fun` ConT (mkName "Expectation"))

  l1 <- do
    p1 <- [p| Left (Failure err $(conP evid []) _) |]
    p2 <- [p| expected |]
    e <- [e| err `shouldBe` expected |]
    return $ Clause [p1, p2] (NormalB e) []

  l2 <- do
    p1 <- [p| Left (Failure err _ f) |]
    p2 <- [p| _ |]
    e <- [e| expectationFailure $ "unexpected sort of error: " ++ f err |]
    return $ Clause [p1, p2] (NormalB e) []

  r <- do
    p1 <- [p| Right x |]
    p2 <- [p| _ |]
    e <- [e| expectationFailure $ "unexpectedly Right value: " ++ show x |]
    return $ Clause [p1, p2] (NormalB e) []

  return [sig, FunD name [l1, l2, r]]
