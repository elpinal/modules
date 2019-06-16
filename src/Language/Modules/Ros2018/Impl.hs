{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Modules.Ros2018.Impl
  ( translate
  ) where

import Control.Monad.Freer hiding (translate)
import Control.Monad.Freer.Error
import Control.Monad.Freer.Fresh
import qualified Data.Text as T

import Language.Modules.Ros2018 hiding (Env)
import Language.Modules.Ros2018.Internal (Builtin(..), BaseType(..), insertValue, emptyEnv, name, Failure, Term, Env)
import Language.Modules.Ros2018.Position

infixr 2 -->
(-->) :: SemanticType -> SemanticType -> SemanticType
ty1 --> ty2 = Function $ fromBody $ Fun ty1 Pure $ fromBody ty2

int :: SemanticType
int = BaseType Int

insertValues :: (?env :: Env f SemanticType) => [(T.Text, SemanticType)] -> Env f SemanticType
insertValues xs = foldl f ?env xs
  where
    f acc (s, ty) =
      let ?env = acc in
        insertValue (name s) ty

instance Builtin SemanticType where
  builtins =
    let ?env = emptyEnv in
      insertValues
        [ ("+", int --> int --> int)
        , ("-", int --> int --> int)
        , ("*", int --> int --> int)
        ]

translate :: Positional Expr -> Either Failure (Either ElaborateError (Term, AbstractType, Purity))
translate e = run $ runError $ runError $ evalFresh 0 $ let ?env = builtins in elaborate e
