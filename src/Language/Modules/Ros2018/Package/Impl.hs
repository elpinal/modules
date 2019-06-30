{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Language.Modules.Ros2018.Package.Impl
  ( runPM
  ) where

import Control.Comonad
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.Trace
import System.Directory
import System.FilePath

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.NDList
import Language.Modules.Ros2018.Package
import Language.Modules.Ros2018.Package.Config
import Language.Modules.Ros2018.Position

data Evidence e where
  EvidPM :: Evidence PMError
  EvidConfig :: Evidence ConfigError
  EvidConfigParse :: Evidence ConfigParseError

data PrettyError = forall e. Display e => PrettyError (Evidence e) e

instance Display PrettyError where
  display (PrettyError _ e) = display e

class Display a => Evidential a where
  evidence :: Evidence a

throwP :: (Evidential e, Member (Error PrettyError) r) => e -> Sem r a
throwP = throw . PrettyError evidence

data PMError
  = NoLib
  | DuplicateModule (Positional Ident) RootRelativePath
  | NoSuchModule (Positional Ident) RootRelativePath
  deriving (Eq, Show)

instance Display PMError where
  display NoLib                    = "no lib.1ml"
  display (DuplicateModule id dir) = display (getPosition id) ++ ": " ++ show dir ++ ": duplicate module: " ++ display id
  display (NoSuchModule id dir)    = display (getPosition id) ++ ": " ++ show dir ++ ": no such module: " ++ display id

instance Evidential PMError where
  evidence = EvidPM

instance Evidential ConfigParseError where
  evidence = EvidConfigParse

traverseDirS :: Member FileSystem r => (FilePath -> Bool) -> FilePath ->
               (T.Text -> Sem r a) -> Sem r (Map.Map RootRelativePath a)
traverseDirS p path f = do
  m <- traverseDir p path f
  sequence m

getMName :: Unit -> Sem r Ident
getMName u = return $ mname u

runPM :: forall r a. Members '[Parser, FileSystem, Trace, Error PrettyError, Reader FilePath] r => Sem (PM ': r) a -> Sem r a
runPM = interpret f
  where
    f :: forall m a. PM m a -> Sem r a
    f (Parse path) = do
      case path of
        "lib.1ml" -> throwP NoLib
        _         -> error "not yet implemented"
    f ReadConfig = do
      txt <- readFileT configFile
      cfg <- either throwP return $ parseConfig configFile txt
      let is = imports cfg
      m <- buildMap is
      return (m, toList $ g . extract <$> is)
        where
          g :: Import -> Ident
          g (Import id _) = extract id
    f (Evaluate t) = trace $ display $ WithName t
    f (GetMapping path) = do
      ask >>= \root -> traverseDirS (".1ml" `isSuffixOf`) (root </> path) $ \content -> parseT content >>= getMName
    f (GetFileName m dir id) = do
      let m' = Map.filter (== extract id) m
      maybe (throwP $ NoSuchModule id dir) f $ Map.minViewWithKey m'
        where
          f :: forall z. ((RootRelativePath, z), FileModuleMap) -> Sem r RootRelativePath
          f ((path, _), rest) = if Map.null rest then return path else throwP $ DuplicateModule id dir

data ConfigError
  = ShadowedImport (Positional Ident)
  deriving (Eq, Show)

instance Display ConfigError where
  display (ShadowedImport id) = display (getPosition id) ++ ": shadowed import: " ++ display id

instance Evidential ConfigError where
  evidence = EvidConfig

buildMap :: Member (Error PrettyError) r => NDList (C Positional Import) -> Sem r ImportMap
buildMap is = foldlM f mempty is
  where
    f :: Member (Error PrettyError) r => ImportMap -> C Positional Import -> Sem r ImportMap
    f m i = do
      let (Import id s) = extract i
      if extract id `Map.member` m
        then throwP $ ShadowedImport id
        else return $ Map.insert (extract id) (toAbsolute $ extract s) m

-- TODO
toAbsolute :: T.Text -> AbsolutePath
toAbsolute = T.unpack

runFileSystemIO :: forall r a. Member (Lift IO) r => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret f
  where
    f :: forall m a. FileSystem m a -> Sem r a
    f (ReadFileT path)       = sendM $ TIO.readFile path
    f (TraverseDir p path f) = do
      entries <- sendM $ filter p <$> listDirectory path
      xs <- sendM $ mapM (\e -> (,) (makeRelative path e) . f <$> TIO.readFile e) entries
      return $ Map.fromList xs

runCatchE :: forall r a. Members '[Error PrettyError] r => Sem (CatchE ': r) a -> Sem r a
runCatchE = interpretH f
  where
    f :: forall m a. CatchE m a -> Tactical CatchE m r a
    f (CatchE m x) = do
      y <- runT m
      raise (runCatchE y) `catch` g
        where
          g :: forall m r. Members '[Error PrettyError] r => PrettyError -> Tactical CatchE m r a
          g (PrettyError EvidPM NoLib) = pureT x -- The absence of lib.1ml is OK.
          g e                          = throw e
