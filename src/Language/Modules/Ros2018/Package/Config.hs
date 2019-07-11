{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Modules.Ros2018.Package.Config
  ( Config(..)
  , Import(..)
  , configFile
  , parseConfig
  , ConfigParseError
  , C(..)
  ) where

import Control.Comonad
import Data.Char
import Data.Coerce
import Data.Monoid
import qualified Data.Text as T
import Data.Void
import GHC.Exts

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.NDList hiding (empty)
import Language.Modules.Ros2018.Position

configFile :: FilePath
configFile = "1ml.package"

-- Provides comparison based on @extract@ of @Comonad@.
newtype C w a = C (w a)
  deriving (Show, Functor)

instance Comonad w => Comonad (C w) where
  extract (C x)  = extract x
  extend f (C x) = C $ extend (\_ -> f $ C x) x

instance (Comonad w, Eq a) => Eq (C w a) where
  C x == C y = extract x == extract y

instance (Comonad w, Ord a) => Ord (C w a) where
  compare (C x) (C y) = extract x `compare` extract y

data Config = Config
  { imports :: NDList (C Positional Import)
  }
  deriving (Eq, Show)

data Import = Import (Positional Ident) (C Positional T.Text)
  deriving Show

instance Eq Import where
  Import _ x == Import _ y = x == y

instance Ord Import where
  compare (Import _ x) (Import _ y) = compare x y

type Parser = Parsec Void T.Text

-- Space consumer.
spaces :: Parser ()
spaces = L.space space1 line empty
  where
    line = L.skipLineComment "--"

lexeme :: Parser a -> Parser (Positional a)
lexeme p = L.lexeme spaces $ do
  start <- getSourcePos
  x <- p
  end <- getSourcePos
  return $ positional (position start end) x

reservedWords :: [T.Text]
reservedWords =
  [ "import"
  ]

reserved :: T.Text -> Parser Position
reserved w = fmap getPosition $ lexeme $ try $ string w *> notFollowedBy alphaNumChar

word :: Parser (Positional T.Text)
word = lexeme $ takeWhile1P (Just "letter") $ (getAny .) $ mconcat $ coerce [isAlphaNum, (`elem` (".,/:-_=+~" :: String))]

config :: Parser Config
config = Config . fromList <$> many p
  where
    p :: Parser (C Positional Import)
    p = do
      pos <- reserved "import"
      as <- word
      from <- word
      return $ C $ positional pos $ Import (ident <$> as) $ C from

newtype ConfigParseError = ConfigParseError (ParseErrorBundle T.Text Void)

instance Display ConfigParseError where
  display (ConfigParseError e) = errorBundlePretty e

parseConfig :: FilePath -> T.Text -> Either ConfigParseError Config
parseConfig = coerce $ parse $ spaces >> config <* eof
