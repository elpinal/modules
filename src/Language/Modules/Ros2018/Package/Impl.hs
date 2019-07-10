{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Language.Modules.Ros2018.Package.Impl
  ( runPM
  ) where

import Control.Comonad
import Control.Monad
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Polysemy
import Polysemy.Error
import Polysemy.Reader
import Polysemy.State
import Polysemy.Trace
import Polysemy.Writer
import System.Directory
import System.FilePath
import System.IO.Error

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Display
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Internal (Generated)
import Language.Modules.Ros2018.NDList
import Language.Modules.Ros2018.Package
import Language.Modules.Ros2018.Package.Config
import Language.Modules.Ros2018.Package.UnitParser
import Language.Modules.Ros2018.Parser (whileParser)
import Language.Modules.Ros2018.Position

data Evidence e where
  EvidPM :: Evidence PMError
  EvidConfig :: Evidence ConfigError
  EvidConfigParse :: Evidence ConfigParseError
  EvidUnitParse :: Evidence UnitParseError

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

instance Evidential UnitParseError where
  evidence = EvidUnitParse

traverseDirS :: Member FileSystem r => (FilePath -> Bool) -> FilePath ->
               (FilePath -> T.Text -> Sem r a) -> Sem r (Map.Map RootRelativePath a)
traverseDirS p path f = do
  m <- traverseDir p path f
  sequence m

getMName :: Unit -> Sem r Ident
getMName u = return $ extract $ mname u

data UsePath = UsePath PackageName RootRelativePath Ident
  deriving (Eq, Ord, Show)

type PMEffs =
 '[ Elab
  , Error PrettyError
  , FileSystem
  , Parser
  , Reader FilePath
  , State (Map.Map UsePath (Generated, AbstractType))
  , Trace
  , VariableGenerator
  , Writer [(Generated, I.Term)]
  ]

runPM :: forall r a. Members PMEffs r => Sem (PM ': r) a -> Sem r a
runPM = interpret f
  where
    f :: forall m a. PM m a -> Sem r a
    f (Parse path) = do
      root <- ask
      txt <- readFileT $ root </> path
      parseT path txt
    f ReadConfig = do
      txt <- readFileT configFile
      cfg <- either throwP return $ parseConfig configFile txt
      let is = imports cfg
      m <- buildMap is
      return (m, g . extract <$> toList is)
        where
          g :: Import -> Ident
          g (Import id _) = extract id
    f (Elaborate xs ts e) = do
      let z f (id, g, aty) env = let ?env = env in
                                 let ?env = I.insertTypes $ reverse $ getAnnotatedKinds aty in
                                 I.insertTempValueWithName (unIdent id) g $ getBody aty
      elab (foldl z id xs) e
    f (Evaluate t) = trace $ display $ WithName t
    f (GetMapping path) = do
      root <- ask
      traverseDirS (".1ml" `isSuffixOf`) (root </> path) $ \fp content -> parseT fp content >>= getMName
    f (GetFileName m dir id) = do
      let m' = Map.filter (== extract id) m
      maybe (throwP $ NoSuchModule id dir) f $ Map.minViewWithKey m'
        where
          f :: forall z. ((RootRelativePath, z), FileModuleMap) -> Sem r RootRelativePath
          f ((path, _), rest) = if Map.null rest then return path else throwP $ DuplicateModule id dir
    f (Register pname dir id aty) = do
      g <- generateVar
      modify $ Map.insert (UsePath pname dir id) (g, aty)
      return g
    f (Emit g t) = tell [(g, t)]

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

runParser :: forall r a. Member (Error PrettyError) r => Sem (Parser ': r) a -> Sem r a
runParser = interpret f
  where
    f :: forall m a. Parser m a -> Sem r a
    f (ParseT fp txt) = either throwP return $ parseUnit whileParser fp txt

runFileSystemIO :: forall r a. Members '[Error PrettyError, Lift IO] r => Sem (FileSystem ': r) a -> Sem r a
runFileSystemIO = interpret f
  where
    f :: forall m a. FileSystem m a -> Sem r a
    f (ReadFileT path) = do
      x <- sendM $ tryIOError $ TIO.readFile path
      either g return x
    f (TraverseDir p path f) = do
      entries <- sendM $ filter p <$> listDirectory path
      xs <- sendM $ forM entries $ \e -> do
        let rpath = makeRelative path e
        (,) rpath . f rpath <$> TIO.readFile e
      return $ Map.fromList xs

    g :: forall a. IOError -> Sem r a
    g e
      | isDoesNotExistError e = throwP NoLib
      | otherwise             = sendM $ ioError e

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
