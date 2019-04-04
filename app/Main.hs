module Main where

import Control.Exception.Safe
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Options.Applicative

import Language.Modules.Ros2018.Parser
import Language.Modules.Ros2018.Display

data InterpretException
  = SyntaxError SyntaxError

instance Exception InterpretException

instance Show InterpretException where
  show (SyntaxError e) = "syntax error: " ++ display e

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
  interpret cmd

instance Display Int where
  display = show

interpret :: (MonadIO m, MonadThrow m) => Command -> m ()
interpret Command
  { filename = fp
  } = do
  txt <- liftIO $ TIO.readFile fp
  case parseText fp txt of
    Right n -> liftIO $ putStrLn $ display n
    Left e -> throw $ SyntaxError e
