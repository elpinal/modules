{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Modules.Ros2018.Internal.Erased.Dynamic
  ( Term(..)
  ) where

import qualified Data.Text as T
import GHC.Generics

import qualified Language.Modules.Ros2018.Internal.Erased as E
import Language.Modules.Ros2018.Internal (Generated, Variable, Label, Record, Literal)
import Language.Modules.Ros2018.Shift

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
  Fix :: Term
  Primitive :: T.Text -> Term
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
  fix = Fix
  primitive = Primitive
