module Main where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Cont
import qualified Data.Text.IO as TIO

import Options.Applicative hiding (Failure)

import Language.Modules.Ros2018 hiding (Type, Env)
import Language.Modules.Ros2018.Display
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

instance Exception InterpretException

instance Show InterpretException where
  show (SyntaxError e)                          = "syntax error: " ++ display e
  show (TranslateError (Failure e _ f))         = "elaboration error: " ++ f e
  show (ElaborateError e)                       = "elaboration error: " ++ display e
  show (InternalTypeError (Failure e _ f))      = "[bug(unsound)] internal type error: " ++ f e
  show (TypeChangeError aty ty (Failure e _ f)) = "[bug(unsound)] type has been changed during elaboration: expected " ++ display (WithName aty) ++ ", but got " ++ display (WithName ty) ++ ": " ++ f e

orThrow :: (MonadThrow m, Exception x) => (e -> x) -> Either e a -> m a
orThrow f = either (throw . f) return

main :: IO ()
main = run

data Command
  = Run
    { parse :: Bool
    , elab :: Bool
    , filename :: FilePath
    }

filenameParser :: Parser FilePath
filenameParser = argument str $ metavar "filename" <> help "Input filename"

information :: InfoMod a
information = fullDesc <> header "1ML_ex"

parser :: Parser Command
parser = subparser $
         command "run" $ flip info fullDesc $ Run <$>
         switch (short 'p' <> long "parse" <> help "Stop after parsing") <*>
         switch (short 'e' <> long "elaborate" <> help "Stop after elaboration") <*>
         filenameParser

run :: (MonadIO m, MonadThrow m) => m ()
run = do
  cmd <- liftIO $ customExecParser (prefs showHelpOnEmpty) $ info (parser <**> helper) $ information
  runContT (interpret cmd) return

interpret :: (MonadIO m, MonadThrow m) => Command -> ContT () m ()
interpret Run
  { filename = fp
  , parse = switchP
  , elab = switchE
  } = callCC $ \exit -> do
  txt <- liftIO $ TIO.readFile fp

  e <- orThrow SyntaxError $ parseText fp txt
  when switchP $ do
    liftIO $ putStrLn $ display e
    exit ()

  (t, aty, p) <- orThrow TranslateError (translate (Y runFailure) e) >>= orThrow ElaborateError
  when switchE $ do
    liftIO $ do
      putStrLn "Term:"
      putStrLn $ display $ WithName t
      putStrLn "Semantic type:"
      putStrLn $ display $ WithName aty
      putStrLn "Purity:"
      putStrLn $ display p
    exit ()

  ty <- orThrow InternalTypeError $ typecheck (toType <$> (builtins :: Env f SemanticType)) t
  liftIO $ putStrLn $ display $ WithName ty
  orThrow (TypeChangeError aty ty) $ equalType (toType aty) ty
