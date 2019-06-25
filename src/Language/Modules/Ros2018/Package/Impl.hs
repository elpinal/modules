{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Language.Modules.Ros2018.Package.Impl
  ( runPM
  ) where

import Polysemy

import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Package

runPM :: forall r a. Members '[Lift IO] r => Sem (PM ': r) a -> Sem r a
runPM = interpretH f
  where
    f :: forall m a. PM m a -> Tactical PM m r a
    f (Evaluate t) = do
      x <- sendM $ putStrLn $ display $ WithName t
      y <- getInitialStateT
      return $ (\_ -> x) <$> y
