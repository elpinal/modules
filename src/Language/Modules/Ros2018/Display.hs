module Language.Modules.Ros2018.Display
  ( Display(..)
  ) where

class Display a where
  display :: a -> String

instance Display Char where
  display ch = [ch]
