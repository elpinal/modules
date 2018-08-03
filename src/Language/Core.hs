{-# LANGUAGE GADTs #-}

module Language.Core
  ( Term(..)
  , eval
  ) where

data Term a where
  Int :: Int -> Term Int
  Abs :: (a -> Term b) -> Term (a -> b)
  App :: Term (a -> b) -> Term a -> Term b

eval :: Term a -> a
eval (Int n) = n
eval (Abs f) = eval . f
eval (App f x) = eval f $ eval x
