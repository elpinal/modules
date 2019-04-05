{-# LANGUAGE DeriveFunctor #-}

module Language.Modules.Ros2018.Position
  ( Position
  , position
  , dummyPos
  , connect
  , Positional(..)
  , positional
  , getPosition
  , fromPositional
  ) where

import Text.Megaparsec.Pos

import Language.Modules.Ros2018.Display

data Position = Position
  { start :: SourcePos
  , end :: SourcePos
  }
  deriving (Eq, Show)

instance Display Position where
  display pos = sourcePosPretty (start pos) ++ ".." ++ sourcePosPretty (end pos)

position :: SourcePos -> SourcePos -> Position
position s e = Position s e

dummyPos :: Position
dummyPos = position (initialPos "") (initialPos "")

connect :: Position -> Position -> Position
connect p1 p2 = Position
  { start = start p1
  , end = end p2
  }

data Positional a = Positional Position a
  deriving (Eq, Show, Functor)

instance Display a => Display (Positional a) where
  display (Positional pos x) = display pos ++ ": " ++ display x

positional :: Position -> a -> Positional a
positional pos x = Positional pos x

getPosition :: Positional a -> Position
getPosition (Positional pos _) = pos

fromPositional :: Positional a -> a
fromPositional (Positional _ x) = x
