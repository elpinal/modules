{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Language.Modules.Ros2018.Impl
  ( translate
  , runM_
  , runMN
  , primitives
  , Y(..)
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Polysemy
import Polysemy.Error hiding (throw)
import qualified Polysemy.Error as E
import Polysemy.Reader
import Polysemy.State

import Language.Modules.Ros2018 hiding (Env)
import Language.Modules.Ros2018.Internal (Builtin(..), BaseType(..), insertValue, emptyEnv, name, Failure, Term, Env)
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Position

data Prim m a where
  LookupPrim :: T.Text -> Prim m AbstractType

makeSem ''Prim

runPrim :: forall r a. Member (Error Failure) r => Sem (Prim ': r) a -> Sem r a
runPrim = interpret f
  where
    f :: forall x m. Prim m x -> Sem r x
    f (LookupPrim s) = maybe (E.throw @Failure $ I.fromSpecific $ I.NoSuchPrimitive s) return $ Map.lookup s primitives

newtype Y k = Y { unY :: forall a. k a -> Either Failure a }

newtype M k a = M
  { unM :: Sem
   '[ Prim
    , Reader (Y k)
    , State Int
    , Error ElaborateError
    , Error Failure
    ] a
  }
  deriving (Functor, Applicative, Monad)

runM_ :: forall k a. Int -> Y k -> M k a -> Either Failure (Either ElaborateError a)
runM_ n f (M m) = run $ runError $ runError $ snd <$> runState n (runReader f $ runPrim m)

runMN:: forall k a. Int -> Y k -> M k a -> Either Failure (Either ElaborateError (a, Int))
runMN n f (M m) = run $ runError $ runError $ (\(x, y) -> (y, x)) <$> runState n (runReader f $ runPrim m)

instance I.FailureM (M k) where
  throwFailure f = M $ E.throw f

instance ErrorM (M k) where
  throwE e       = M $ E.throw e
  catchE (M m) f = M $ catch m $ unM . f

instance PrimAM (M k) where
  getATypeOfPrim s = M $ lookupPrim s

instance I.FailureM k => RunFailureM (M k) where
  runF = M $ do
    f <- ask @(Y k)
    return $ Z $ unY f

fresh :: M k Int
fresh = M $ do
  n <- get
  modify (+ 1)
  return n

instance FreshM (M k) where
  -- Generates a new name in the form: "?d28".
  freshName = do
    n <- fresh
    return $ name $ "?d" <> T.pack (show n)
  freshGenerated = I.generated <$> fresh

primitives :: Map.Map T.Text AbstractType
primitives =
  [ ("and", fromBody $ bool --> bool --> bool)
  , ("int_compare", fromBody $ ordering --&> int --> int --> tvarS 0)
  ]
  where
    ordering :: AbstractType
    ordering = quantify [I.unannotated I.Base] $ Structure
      [ (I.label "t", AbstractType $ fromBody $ tvarS 0)
      , (I.label "LT", tvarS 0)
      , (I.label "EQ", tvarS 0)
      , (I.label "GT", tvarS 0)
      ]

infixr 2 -->
(-->) :: SemanticType -> SemanticType -> SemanticType
ty1 --> ty2 = Function $ fromBody $ Fun ty1 Pure $ fromBody ty2

infixr 2 --&>
(--&>) :: Quantification f => f SemanticType -> SemanticType -> SemanticType
ty1 --&> ty2 = Function $ quantify (getAnnotatedKinds ty1) $ Fun (getBody ty1) Pure $ fromBody ty2

bool :: SemanticType
bool = BaseType Bool

int :: SemanticType
int = BaseType Int

insertValues :: (?env :: Env f SemanticType) => [(T.Text, SemanticType)] -> Env f SemanticType
insertValues xs = foldl f ?env xs
  where
    f acc (s, ty) =
      let ?env = acc in
        insertValue (name s) ty

tvarS :: Int -> SemanticType
tvarS = SemanticPath . fromVariable . I.variable

instance Builtin SemanticType where
  builtins =
    let ?env = emptyEnv in
      insertValues
        [ ("+", int --> int --> int)
        , ("-", int --> int --> int)
        , ("*", int --> int --> int)
        ]

translate :: I.FailureM k => Y k -> Positional Expr -> Either Failure (Either ElaborateError (Term, AbstractType, Purity))
translate f e = runM_ 0 f $ let ?env = builtins in elaborate e
