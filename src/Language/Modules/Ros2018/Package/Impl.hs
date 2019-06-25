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
import qualified Data.Text.IO as TIO
import Polysemy
import Polysemy.Error

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.NDList
import Language.Modules.Ros2018.Package
import Language.Modules.Ros2018.Package.Config
import Language.Modules.Ros2018.Position

data PrettyError = forall e. Display e => PrettyError e

instance Display PrettyError where
  display (PrettyError e) = display e

runPM :: forall r a. Members '[Lift IO, Error PrettyError] r => Sem (PM ': r) a -> Sem r a
runPM = interpretH f
  where
    f :: forall m a. PM m a -> Tactical PM m r a
    f (Evaluate t) = do
      x <- sendM $ putStrLn $ display $ WithName t
      state <- getInitialStateT
      return $ (\_ -> x) <$> state
    f ReadConfig = do
      txt <- sendM $ TIO.readFile configFile
      cfg <- either (throw . PrettyError) return $ parseConfig configFile txt
      let is = imports cfg
      m <- buildMap is
      state <- getInitialStateT
      return $ const (m, toList $ g . extract <$> is) <$> state
        where
          g :: Import -> Ident
          g (Import id _) = extract id

data ConfigError
  = ShadowedImport (Positional Ident)
  deriving (Eq, Show)

instance Display ConfigError where
  display (ShadowedImport id) = display (getPosition id) ++ ": shadowed import: " ++ display id

buildMap :: Member (Error PrettyError) r => NDList (C Positional Import) -> Sem r ImportMap
buildMap is = foldlM f mempty is
  where
    f :: Member (Error PrettyError) r => ImportMap -> C Positional Import -> Sem r ImportMap
    f m i = do
      let (Import id s) = extract i
      if extract id `Map.member` m
        then throw $ PrettyError $ ShadowedImport id
        else return $ Map.insert (extract id) (toAbsolute $ extract s) m

-- TODO
toAbsolute :: T.Text -> AbsolutePath
toAbsolute = T.unpack
