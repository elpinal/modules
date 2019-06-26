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
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Polysemy
import Polysemy.Error
import Polysemy.Trace

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
  deriving (Eq, Show)

instance Display PMError where
  display NoLib = "no lib.1ml"

instance Evidential PMError where
  evidence = EvidPM

instance Evidential ConfigParseError where
  evidence = EvidConfigParse

runPM :: forall r a. Members '[FileSystem, Trace, Error PrettyError] r => Sem (PM ': r) a -> Sem r a
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
