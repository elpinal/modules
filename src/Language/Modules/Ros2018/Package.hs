{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Modules.Ros2018.Package
  ( buildMain

  , PM(..)
  , Parser(..)
  , CatchE(..)
  , FileSystem(..)
  , VariableGenerator(..)

  , Unit(..)
  , Visibility(..)

  , evaluate
  , parse

  , parseT

  , readFileT
  , traverseDir

  , generateVar

  , RootRelativePath
  , AbsolutePath
  , ImportMap
  , FileModuleMap
  , PackageName
  ) where

import Control.Comonad
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Polysemy
import System.FilePath (takeDirectory)

import Language.Modules.Ros2018 (Expr, Ident, AbstractType, Purity)
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal (Generated)
import Language.Modules.Ros2018.NDList
import Language.Modules.Ros2018.Position

data Unit = Unit
  { mname :: Positional Ident
  , uses :: [Positional T.Text]
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

type FileModuleMap = Map.Map RootRelativePath Ident

type PackageName = Ident

-- Package manager
data PM m a where
  Parse :: RootRelativePath -> PM m Unit
  ReadConfig :: PM m (ImportMap, [Ident])
  Elaborate :: [Positional T.Text] -> Positional Expr -> PM m (I.Term, AbstractType, Purity)
  Evaluate :: I.Term -> PM m ()
  -- Returns not necessarily injective, filename-to-module-identifier mapping.
  GetMapping :: RootRelativePath -> PM m FileModuleMap
  GetFileName :: FileModuleMap -> RootRelativePath -> Positional Ident -> PM m RootRelativePath
  -- -- May return a variable denoting an external library.
  -- ResolveExternal :: ImportMap -> Positional T.Text -> PM m I.Term
  ElaborateExternal :: Ident -> PM m I.Term
  Combine :: [I.Term] -> Maybe I.Term -> I.Term -> PM m I.Term
  Register :: PackageName -> RootRelativePath -> Ident -> PM m Generated
  Emit :: Generated -> I.Term -> PM m ()

makeSem ''PM

data Parser m a where
  ParseT :: FilePath -> T.Text -> Parser m Unit

makeSem ''Parser

data FileSystem m a where
  ReadFileT :: FilePath -> FileSystem m T.Text
  -- @traverseDir p dir f@ traverses a directory @dir@, applying @f@ to each file which satisfies a predicate @p@.
  TraverseDir :: (FilePath -> Bool) -> FilePath ->
                 (FilePath -> T.Text -> a) -> FileSystem m (Map.Map RootRelativePath a)

makeSem ''FileSystem

data VariableGenerator m a where
  GenerateVar :: VariableGenerator m Generated

makeSem ''VariableGenerator

data CatchE m a where
  CatchE :: m a -> a -> CatchE m a

makeSem ''CatchE

mname' :: Unit -> Ident
mname' = extract . mname

buildMain :: Members '[PM, CatchE] r => Sem r ()
buildMain = do
  (m, ids) <- readConfig
  tms <- mapM elaborateExternal ids
  u <- parse "main.1ml"
  mt <- fmap Just (buildLib $ mname' u) `catchE` Nothing
  let ts = uses u
  -- vs <- mapM (resolveExternal m) ts
  (t, _, _) <- elaborate ts $ body u
  combine tms mt t >>= evaluate

buildLib :: Member PM r => PackageName -> Sem r I.Term
buildLib id = do
  u <- parse "lib.1ml"
  let ts = uses u
  m <- getMapping "."
  let sms = submodules u
  ps <- mapM (getFileName m "." . snd) sms
  gs <- mapM (build id) ps
  (t, _, _) <- elaborate ts $ body u
  _ <- register id "." $ mname' u
  return t

-- TODO
stripExt :: RootRelativePath -> RootRelativePath
stripExt p =
  if ".1ml" `isSuffixOf` p
    then drop 4 p
    else error "stripExt"

build :: Member PM r => PackageName -> RootRelativePath -> Sem r Generated
build id p = do
  u <- parse p
  let ts = uses u
  m <- getMapping $ stripExt p
  let sms = submodules u
  ps <- mapM (getFileName m (stripExt p) . snd) sms
  gs <- mapM (build id) ps
  (t, aty, purity) <- elaborate ts $ body u
  g <- register id (takeDirectory p) $ mname' u
  emit g t
  return g
