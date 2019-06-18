{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Language.Modules.Ros2018.Impl
  ( translate
  , runM_
  ) where

import qualified Data.Text as T
import Polysemy
import Polysemy.Error
import Polysemy.State

import Language.Modules.Ros2018 hiding (Env)
import Language.Modules.Ros2018.Internal (Builtin(..), BaseType(..), insertValue, emptyEnv, name, Failure, Term, Env)
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Position

newtype M a = M { unM :: Sem '[State Int, Error ElaborateError, Error Failure] a }
  deriving (Functor, Applicative, Monad)

runM_ :: Int -> M a -> Either Failure (Either ElaborateError a)
runM_ n (M m) = run $ runError $ runError $ snd <$> runState n m

instance I.FailureM M where
  throwFailure f = M $ throw f

instance ErrorM M where
  throwE e       = M $ throw e
  catchE (M m) f = M $ catch m $ unM . f

fresh :: M Int
fresh = M $ do
  n <- get
  modify (+ 1)
  return n

instance FreshM M where
  -- Generates a new name in the form: "?d28".
  freshName = do
    n <- fresh
    return $ name $ "?d" <> T.pack (show n)
  freshGenerated = I.generated <$> fresh

infixr 2 -->
(-->) :: SemanticType -> SemanticType -> SemanticType
ty1 --> ty2 = Function $ fromBody $ Fun ty1 Pure $ fromBody ty2

infixr 2 --&>
(--&>) :: Quantification f => f SemanticType -> SemanticType -> SemanticType
ty1 --&> ty2 = Function $ quantify (getAnnotatedKinds ty1) $ Fun (getBody ty1) Pure $ fromBody ty2

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
        , ("Ordering", AbstractType ordering)
        , ("Int", Structure
            [ (I.label "compare", ordering --&> int --> int --> tvarS 0)
            ])
        ]
    where
      ordering = quantify [I.unannotated I.Base] $ Structure
            [ (I.label "t", AbstractType $ fromBody $ tvarS 0)
            , (I.label "LT", tvarS 0)
            , (I.label "EQ", tvarS 0)
            , (I.label "GT", tvarS 0)
            ]

translate :: Positional Expr -> Either Failure (Either ElaborateError (Term, AbstractType, Purity))
translate e = runM_ 0 $ let ?env = builtins in elaborate e
