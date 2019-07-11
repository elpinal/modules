module Main where

import Control.Exception.Safe
import Control.Monad.Cont
import qualified Data.Text.IO as TIO
import System.Directory

import Options.Applicative hiding (Failure)

import Language.Modules.Ros2018 hiding (Type, Env)
import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Package.Impl
import Language.Modules.Ros2018.Impl
import Language.Modules.Ros2018.Internal hiding (throw)
import Language.Modules.Ros2018.Internal.Impl
import Language.Modules.Ros2018.Parser

data InterpretException
  = SyntaxError SyntaxError
  | TranslateError Failure
  | ElaborateError ElaborateError
  | InternalTypeError Failure
  | TypeChangeError AbstractType Type Failure
  | PrettyError_ PrettyError

instance Exception InterpretException

instance Show InterpretException where
  show (SyntaxError e)                          = "syntax error: " ++ display e
  show (TranslateError (Failure e _ f))         = "elaboration error: " ++ f e
  show (ElaborateError e)                       = "elaboration error: " ++ display e
  show (InternalTypeError (Failure e _ f))      = "[bug(unsound)] internal type error: " ++ f e
  show (TypeChangeError aty ty (Failure e _ f)) = "[bug(unsound)] type has been changed during elaboration: expected " ++ display (WithName aty) ++ ", but got " ++ display (WithName ty) ++ ": " ++ f e
  show (PrettyError_ e)                         = display e

orThrow :: (MonadThrow m, Exception x) => (e -> x) -> Either e a -> m a
orThrow f = either (throw . f) return

main :: IO ()
main = run

data Command
  = Run
    { filename :: FilePath
    }

filenameParser :: Parser FilePath
filenameParser = argument str $ metavar "filename" <> help "Input filename"

information :: InfoMod a
information = fullDesc <> header "1ML_ex"

parser :: Parser Command
parser = subparser $
         command "run" $ flip info fullDesc $ Run <$>
         filenameParser

run :: (MonadIO m, MonadThrow m) => m ()
run = do
  cmd <- liftIO $ customExecParser (prefs showHelpOnEmpty) $ info (parser <**> helper) $ information
  runContT (build cmd) return

build :: (MonadIO m, MonadThrow m) => Command -> ContT () m ()
build Run{} = do
  root <- liftIO getCurrentDirectory
  x <- liftIO (runPM root $ H buildMain) >>= orThrow PrettyError_ >>= orThrow TranslateError
  -- return $ fst x
  return ()

interpret :: (MonadIO m, MonadThrow m) => Command -> ContT () m ()
interpret Run
  { filename = fp
  } = do
  txt <- liftIO $ TIO.readFile fp

  e <- orThrow SyntaxError $ parseText fp txt

  (t, aty, _) <- orThrow TranslateError (translate (Y runFailure) e) >>= orThrow ElaborateError

  ty <- orThrow InternalTypeError $ typecheck (toType <$> (builtins :: Env f SemanticType)) t
  liftIO $ putStrLn $ display $ WithName ty
  orThrow (TypeChangeError aty ty) $ equalType (toType aty) ty
