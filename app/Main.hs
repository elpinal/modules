module Main where

import Control.Exception.Safe
import Control.Monad.Cont
import System.Directory

import Options.Applicative hiding (Failure)

import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Package.Impl
import Language.Modules.Ros2018.Internal hiding (throw)

data InterpretException
  = TranslateError Failure
  | PrettyError_ PrettyError

instance Exception InterpretException

instance Show InterpretException where
  show (TranslateError (Failure e _ f)) = "elaboration error: " ++ f e
  show (PrettyError_ e)                 = display e

orThrow :: (MonadThrow m, Exception x) => (e -> x) -> Either e a -> m a
orThrow f = either (throw . f) return

main :: IO ()
main = run

data Command = Run

information :: InfoMod a
information = fullDesc <> header "1ML_ex"

parser :: Parser Command
parser = subparser $
         command "run" $ flip info fullDesc $ pure Run

run :: (MonadIO m, MonadThrow m) => m ()
run = do
  cmd <- liftIO $ customExecParser (prefs showHelpOnEmpty) $ info (parser <**> helper) information
  runContT (build cmd) return

build :: (MonadIO m, MonadThrow m) => Command -> ContT () m ()
build Run{} = do
  root <- liftIO getCurrentDirectory
  liftIO (runPM root $ H buildMain) >>= orThrow PrettyError_ >>= orThrow TranslateError
