{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Modules.Ros2018.Internal.Erased.Dynamic
  ( Term(..)
  , evaluate
  ) where

import Data.Maybe
import qualified Data.Text as T
import GHC.Generics

import qualified Language.Modules.Ros2018.Internal.Erased as E
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal (Generated, Variable, Label, Record, Literal, variable)
import Language.Modules.Ros2018.Shift

data Passed
  = Zero
  | One
  | Two
  deriving (Eq, Show)
  deriving Shift via Fixed Passed

suc :: Passed -> Passed
suc Zero = One
suc One  = Two
suc Two  = error "suc"

data Arith
  = Add
  | Sub
  | Mul
  deriving (Eq, Show)
  deriving Shift via Fixed Arith

data Term where
  Lit :: Literal -> Term
  Var :: Variable -> Term
  GVar :: Generated -> Term
  Abs :: Term -> Term
  App :: Term -> Term -> Term
  TmRecord :: Record Term -> Term
  Proj :: Term -> Label -> Term
  LetG :: Generated -> Term -> Term -> Term
  If :: Term -> Term -> Term -> Term
  Let :: [Term] -> Term -> Term
  Fix :: Passed -> Term
  Primitive :: T.Text -> Term
  Arith :: Arith -> Term -> Term -> Term
  And1 :: Term -> Term
  IntCompare1 :: Term -> Term
  deriving Generic

instance Shift T.Text where
  shiftAbove _ _ = id

instance Shift Term where
  shiftAbove c d (Abs t)    = Abs $ shiftAbove (c + 1) d t
  shiftAbove c d (Let ts t) = Let ts $ shiftAbove (c + length ts) d t
  shiftAbove c d t          = to $ gShiftAbove c d $ from t

instance E.Term Term where
  lit = Lit
  var = Var
  gvar = GVar
  abs_ = Abs
  app = App
  tmRecord = TmRecord
  proj = Proj
  poly t = Abs $ shift 1 t
  unpack Nothing t1 t2 = E.let_ [t1] t2
  unpack (Just g) t1 t2 = LetG g t1 t2
  if_ = If
  let_ = Let
  fix = Fix Zero
  primitive = Primitive

subst :: Either Generated Int -> Term -> Term -> Term
subst j by = f 0
  where
    f _ t @ Lit{} = t
    f _ t @ Fix{} = t
    f _ t @ Primitive{} = t
    f _ t @ And1{} = t
    f _ t @ IntCompare1{} = t
    f c (Var v) =
      case j of
        Right j | variable (c + j) == v -> shift c by
        _                               -> Var v
    f c (GVar g1) =
      case j of
        Left g2 | g1 == g2 -> shift c by
        _                  -> GVar g1
    f c (Abs t) = Abs $ f (c + 1) t
    f c (App t1 t2) = App (f c t1) (f c t2)
    f c (TmRecord r) = TmRecord $ f c <$> r
    f c (Proj t l) = Proj (f c t) l
    f c (LetG g t1 t2) = LetG g (f c t1) (f c t2)
    f c (If t1 t2 t3) = If (f c t1) (f c t2) (f c t3)
    f c (Let ts t) = Let (f c <$> ts) $ f (c + 1) t
    f c (Arith a x y) = Arith a (f c x) (f c y)

substTop :: Term -> Term -> Term
substTop by t = shift (-1) $ subst (Right 0) (shift 1 by) t

fromBool :: Term -> Bool
fromBool (Lit (I.LBool b)) = b
fromBool _                 = error "fromBool"

fromInt :: Term -> Int
fromInt (Lit (I.LInt b)) = b
fromInt _                = error "fromInt"

eval :: Term -> Term
eval t @ Lit{} = t
eval t @ Abs{} = t
eval t @ Fix{} = t
eval t @ Primitive{} = t
eval t @ And1{} = t
eval t @ IntCompare1{} = t
eval (App y z) =
  let t1 = eval y in
  let t2 = eval z in
  case t1 of
    Abs t       -> substTop t2 t
    Fix Two     -> eval $ App t2 $ Abs $ App (App (Fix Two) t2) $ Var $ variable 0
    Fix p       -> Fix $ suc p
    Primitive x -> prim x t2
    And1 t      -> Lit $ I.LBool $ fromBool t && fromBool t2
    -- TODO: IntCompare1 t -> 
    _ -> error "not function"
eval Var{} = error "variable"
eval GVar{} = error "generated variable"
eval (TmRecord r) = TmRecord $ eval <$> r
eval (Proj t l) =
  case eval t of
    TmRecord r -> fromMaybe (error "unbound label") $ I.projRecord l r
    _ -> error "not record"
eval (LetG g t1 t2) = eval $ subst (Left g) (eval t1) t2
eval (If t1 t2 t3) =
  case eval t1 of
    Lit (I.LBool b) -> if b then eval t2 else eval t3
    _               -> error "not boolean value"
eval (Let ts t) =
  let xs = shift (length ts) . eval <$> ts in
  shift (-length ts) $ foldl (\t (i, x) -> subst (Right i) x t) t $ zip [0..] $ reverse xs
eval (Arith a x y) =
  let m = fromInt x in
  let n = fromInt y in
  let i = Lit . I.LInt in
  case a of
    Add -> i $ m + n
    Sub -> i $ m - n
    Mul -> i $ m * n

prim :: T.Text -> Term -> Term
prim "and" t         = And1 t
prim "int_compare" t = IntCompare1 t
prim txt _           = error $ "unknown primitive: " ++ show txt

evaluate :: Term -> Term
evaluate t = eval $ Let [f Add, f Sub, f Mul] t
  where
    f a = Abs $ Abs $ Arith a (v 1) (v 0)
    v = Var . variable
