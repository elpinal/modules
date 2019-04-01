module Language.Modules.Ros2018.Display
  ( Display(..)
  ) where

class Display a where
  display :: a -> String
  displaysPrec :: Int -> a -> ShowS

  display x = displaysPrec 0 x ""
  displaysPrec n x s = display x ++ s

instance Display Char where
  display ch = [ch]
