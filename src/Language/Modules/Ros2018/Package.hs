{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Modules.Ros2018.Package
  ( buildMain

  , PM(..)
  , CatchE(..)
  , Unit(..)

  , evaluate
  , parse

  , AbsolutePath
  , ImportMap
  ) where

import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Polysemy

import Language.Modules.Ros2018 (Expr, Ident, AbstractType)
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.NDList
import Language.Modules.Ros2018.Position

data Unit = Unit
  { uses :: [Positional T.Text]
  , submodules :: NDList (Visibility, Positional Ident)
  , body :: Positional Expr
  }

data Visibility
  = Private
  | Public
  deriving (Eq, Show)

type RootRelativePath = FilePath

type AbsolutePath = FilePath

type ImportMap = Map.Map Ident AbsolutePath

-- Package manager
data PM m a where
  Parse :: RootRelativePath -> PM m Unit
  ReadConfig :: PM m (ImportMap, [Ident])
  Elaborate :: Positional Expr -> PM m (I.Term, AbstractType)
  Evaluate :: I.Term -> PM m ()
  -- Returns not necessarily injective mapping.
  GetMapping :: RootRelativePath -> PM m (Map.Map RootRelativePath Ident)
  GetFileName :: RootRelativePath -> Positional Ident -> PM m RootRelativePath
  -- May return a variable denoting an external library.
  ResolveExternal :: ImportMap -> Positional T.Text -> PM m I.Term
  ElaborateExternal :: Ident -> PM m I.Term
  Combine :: [I.Term] -> Maybe I.Term -> I.Term -> PM m I.Term

makeSem ''PM

data CatchE m a where
  CatchE :: m a -> a -> CatchE m a

makeSem ''CatchE

buildMain :: Members '[PM, CatchE] r => Sem r ()
buildMain = do
  (m, ids) <- readConfig
  tms <- mapM elaborateExternal ids
  mt <- fmap Just buildLib `catchE` Nothing
  u <- parse "main.1ml"
  let ts = uses u
  vs <- mapM (resolveExternal m) ts
  (t, _) <- elaborate $ body u
  combine tms mt t >>= evaluate

buildLib :: Member PM r => Sem r I.Term
buildLib = do
  u <- parse "lib.1ml"
  let ts = uses u
  m <- getMapping "."
  let sms = submodules u
  ps <- mapM (getFileName "." . snd) sms
  mapM_ build ps
  (t, _) <- elaborate $ body u
  return t

-- TODO
stripExt :: RootRelativePath -> RootRelativePath
stripExt p =
  if ".1ml" `isSuffixOf` p
    then drop 4 p
    else error "stripExt"

build :: Member PM r => RootRelativePath -> Sem r ()
build p = do
  u <- parse p
  let ts = uses u
  m <- getMapping $ stripExt p
  let sms = submodules u
  ps <- mapM (getFileName (stripExt p) . snd) sms
  mapM_ build ps
  (t, _) <- elaborate $ body u
  evaluate t
