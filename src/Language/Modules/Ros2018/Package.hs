{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Modules.Ros2018.Package
  ( buildMain

  , PM(..)
  , Elab(..)
  , Parser(..)
  , FileSystem(..)
  , VariableGenerator(..)

  , Unit(..)
  , Visibility(..)

  , UsePath(..)

  , RootRelativePath
  , AbsolutePath
  , ImportMap
  , FileModuleMap
  , PackageName

  , U(..)
  ) where

import Control.Comonad
import Data.Coerce
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import GHC.Exts (IsList(..))
import System.FilePath (takeDirectory)

import Language.Modules.Ros2018 (Env, Expr, Ident, AbstractType, Purity)
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal (Generated)
import qualified Language.Modules.Ros2018.Internal.Erased as E
import Language.Modules.Ros2018.NDList
import Language.Modules.Ros2018.Package.Config (C(..))
import Language.Modules.Ros2018.Position

data Unit = Unit
  { mname :: Positional Ident
  , uses :: [Positional T.Text]
  , submodules :: NDList (C ((,) Visibility) (C Positional Ident))
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

newtype U = U { unU :: forall t. E.Term t => t }

data UsePath
  = UsePath PackageName RootRelativePath Ident
  | Root PackageName
  deriving (Eq, Ord, Show)

-- Package manager
class Monad m => PM m where
  parse :: RootRelativePath -> m Unit
  readConfig :: m (ImportMap, [Ident])
  elaborate :: [(Ident, Generated, AbstractType)] -> [Positional T.Text] -> Positional Expr -> m (I.Term, AbstractType, Purity)
  evaluate :: U -> m ()
  -- Returns not necessarily injective, filename-to-module-identifier mapping.
  getMapping :: RootRelativePath -> m FileModuleMap
  getFileName :: FileModuleMap -> RootRelativePath -> Positional Ident -> m RootRelativePath
  -- -- May return a variable denoting an external library.
  -- resolveExternal :: ImportMap -> Positional T.Text -> m I.Term
  -- elaborateExternal :: Ident -> m I.Term
  combine :: PackageName -> [I.Term] -> Maybe I.Term -> I.Term -> m U
  register :: UsePath -> AbstractType -> m Generated
  emit :: Generated -> I.Term -> m ()
  catchE :: m a -> a -> m a

class Monad m => Elab m where
  elab :: (Env -> Env) -> Positional Expr -> m (I.Term, AbstractType, Purity)

class Monad m => Parser m where
  parseT :: FilePath -> T.Text -> m Unit

class Monad m => FileSystem m where
  readFileT :: FilePath -> m T.Text
  -- @traverseDir p dir f@ traverses a directory @dir@, applying @f@ to each file which satisfies a predicate @p@.
  -- If @dir@ does not exist, @Nothing@ is returned.
  traverseDir :: (FilePath -> Bool) -> FilePath ->
                 (FilePath -> T.Text -> a) -> m (Maybe (Map.Map RootRelativePath a))

class Monad m => VariableGenerator m where
  generateVar :: m Generated

mname' :: Unit -> Ident
mname' = extract . mname

buildMain :: PM m => m ()
buildMain = do
  (m, ids) <- readConfig `catchE` (mempty, [])
  -- tms <- mapM elaborateExternal ids
  u <- parse "main.1ml"
  let pn = mname' u
  mt <- fmap Just (buildLib pn) `catchE` Nothing
  let ts = uses u
  -- vs <- mapM (resolveExternal m) ts
  (t, _, _) <- elaborate [] ts $ body u
  combine pn [] mt t >>= evaluate

buildLib :: PM m => PackageName -> m I.Term
buildLib id = do
  u <- parse "lib.1ml"
  let ts = uses u
  m <- getMapping "."
  let sms = submodules u
  ps <- mapM (getFileName m "." . coerce . extract) $ toList sms
  xs <- mapM (build id) ps
  (t, aty, _) <- elaborate xs ts $ body u
  _ <- register (Root id) aty -- TODO: Perhaps no need to register if the module is marked as "private".
  return t

-- TODO
stripExt :: RootRelativePath -> RootRelativePath
stripExt p =
  if ".1ml" `isSuffixOf` p
    then take (length p - 4) p
    else error "stripExt"

build :: PM m => PackageName -> RootRelativePath -> m (Ident, Generated, AbstractType)
build id p = do
  u <- parse p
  let ts = uses u
  m <- getMapping $ stripExt p
  let sms = submodules u
  ps <- mapM (getFileName m (stripExt p) . coerce . extract) $ toList sms
  xs <- mapM (build id) ps
  (t, aty, _) <- elaborate xs ts $ body u
  g <- register (UsePath id (takeDirectory p) $ mname' u) aty -- TODO: Perhaps no need to register if the module is marked as "private".
  emit g t
  return (mname' u, g, aty)
