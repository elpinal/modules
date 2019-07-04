{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Modules.Ros2018.Internal.Erased
  ( Term(..)
  ) where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import GHC.Exts

import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Internal (Generated, Variable, Label, Record, Literal)

-- | A type-erased term.
class Term t where
  lit :: Literal -> t
  var :: Variable -> t
  gvar :: Generated -> t
  abs_ :: t -> t
  app :: t -> t -> t
  tmRecord :: Record t -> t
  proj :: t -> Label -> t
  poly :: t -> t
  inst :: t -> t
  pack :: t -> t
  unpack :: Maybe Generated -> t -> t -> t
  if_ :: t -> t -> t -> t
  let_ :: [t] -> t -> t
  fix :: t
  primitive :: T.Text -> t

  pack x = x
  poly x = abs_ x -- TODO: shift?
  inst x = app x $ tmRecord []

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True  = parens
parensIf False = id

prettyDisplay :: Display a => a -> Doc ann
prettyDisplay = pretty . display

instance Term (Int -> Doc ann) where
  lit l _       = prettyDisplay l
  var v _       = prettyDisplay v
  gvar g _      = prettyDisplay g
  abs_ x n      = parensIf (4 <= n) $ "λ." <+> x 0
  app x y n     = parensIf (5 <= n) $ x 4 <+> y 5
  tmRecord r _  = braces $ hsep $ punctuate comma $ (\(l, t) -> hsep [prettyDisplay l, "=", t 0]) <$> toList r
  proj x l _    = hcat [x 9, ".", prettyDisplay l]
  poly x n      = parensIf (4 <= n) $ "Λ." <+> x 0
  inst x n      = parensIf (5 <= n) $ x 4 <+> "[_]"
  if_ x y z n   = parensIf (4 <= n) $ hsep ["if", x 0, "then", y 0, "else", z 0]
  let_ xs y n   = parensIf (4 <= n) $ hsep ["let", hsep $ punctuate semi $ map ($ 0) xs, "in", y 0]
  fix _         = "fix"
  primitive t n = parensIf (5 <= n) $ "primitive" <+> viaShow t
  unpack Nothing x y n  = let_ [x] y n
  unpack (Just g) x y n = parensIf (4 <= n) $ hsep ["let", prettyDisplay g, "=", x 0, "in", y 0]
