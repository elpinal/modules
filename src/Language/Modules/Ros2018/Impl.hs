{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Modules.Ros2018.Impl
  ( translate
  ) where

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Error
import Control.Monad.Freer.Fresh

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Internal (Builtin(..), BaseType(..), insertValue, emptyEnv, name, Failure, Term)
import Language.Modules.Ros2018.Position

infixr 2 -->
(-->) :: SemanticType -> SemanticType -> SemanticType
ty1 --> ty2 = Function $ fromBody $ Fun ty1 Pure $ fromBody ty2

int = BaseType Int

instance Builtin SemanticType where
  builtins =
    let ?env = emptyEnv in
    let ?env = insertValue (name "+") $ int --> int --> int in
      ?env

translate :: Positional Expr -> Either Failure (Either ElaborateError (Term, AbstractType, Purity))
translate e = run $ runError $ runError $ evalFresh 0 $ let ?env = builtins in elaborate e
