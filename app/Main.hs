module Main where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.Cont
import qualified Data.Text.IO as TIO

import Options.Applicative

import Language.Modules.Ros2018.Parser
import Language.Modules.Ros2018.Display

data InterpretException
  = SyntaxError SyntaxError

instance Exception InterpretException

instance Show InterpretException where
  show (SyntaxError e) = "syntax error: " ++ display e

orThrow :: (MonadThrow m, Exception x) => (e -> x) -> Either e a -> m a
orThrow f = either (throw . f) return

main :: IO ()
main = run

data Command = Command
  { parse :: Bool
  , filename :: FilePath
  }

filenameParser :: Parser FilePath
filenameParser = argument str $ metavar "filename" <> help "Input filename"

information :: InfoMod a
information = fullDesc <> progDesc "1ML_ex interpreter" <> header "1ML_ex"

parser :: Parser Command
parser = Command <$>
         switch (short 'p' <> long "parse" <> help "Stop after parsing") <*>
         filenameParser

run :: (MonadIO m, MonadThrow m) => m ()
run = do
  cmd <- liftIO $ customExecParser (prefs showHelpOnEmpty) $ info (parser <**> helper) $ information
  runContT (interpret cmd) return

interpret :: (MonadIO m, MonadThrow m) => Command -> ContT () m ()
interpret Command
  { filename = fp
  , parse = p
  } = callCC $ \exit -> do
  txt <- liftIO $ TIO.readFile fp
  b <- orThrow SyntaxError $ parseText fp txt
  when p $ do
    liftIO $ putStrLn $ display b
    exit ()
