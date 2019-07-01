{-# LANGUAGE OverloadedStrings #-}

module Language.Modules.Ros2018.Package.UnitParser
  ( parseUnit
  , UnitParseError(..)
  ) where

import Control.Monad
import Data.Coerce
import qualified Data.Text as T
import Data.Void
import GHC.Exts

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.NDList hiding (empty)
import Language.Modules.Ros2018.Package hiding (Parser(..), parse)
import Language.Modules.Ros2018.Package.Config (C(..))
import Language.Modules.Ros2018.Parser (identifier)
import Language.Modules.Ros2018.Position

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

symbol :: T.Text -> Parser Position
symbol = fmap getPosition . lexeme . string

reservedWords :: [T.Text]
reservedWords =
  [ "module"
  , "submodule"
  , "use"
  , "private"
  ]

reserved :: T.Text -> Parser Position
reserved w = fmap getPosition $ lexeme $ try $ string w *> notFollowedBy alphaNumChar

moduleName :: Parser (Positional Ident)
moduleName = reserved "module" >> identifier -- TODO: @identifier@ can parse reserved words like "module" and "private".

use :: Parser (Positional T.Text)
use = do
  start <- reserved "use"
  _ <- char '"'
  txt <- takeWhileP (Just "character") (/= '"')
  end <- symbol "\""
  return $ positional (connect start end) txt

submodule :: Parser (C ((,) Visibility) (C Positional Ident))
submodule = do
  _ <- reserved "submodule"
  v <- option Public $ const Private <$> reserved "private"
  id <- identifier
  return $ C (v, C id)

manyND :: Ord a => Parser a -> Parser (Maybe (NDList a))
manyND p = go return
  where
    go f = do
      r <- optional p
      case r of
        Nothing -> return $ f $ fromList []
        Just  x -> go $ f <=< (x <&)

unit :: Parser (Positional Expr) -> Parser Unit
unit p = do
  id <- moduleName
  us <- many use
  sms <- manyND submodule >>= maybe (fail "duplicate submodule") return
  b <- p
  return Unit
    { mname = id
    , uses = us
    , submodules = coerce sms
    , body = b
    }

newtype UnitParseError = UnitParseError (ParseErrorBundle T.Text Void)

instance Display UnitParseError where
  display (UnitParseError e) = errorBundlePretty e

parseUnit :: Parser (Positional Expr) -> FilePath -> T.Text -> Either UnitParseError Unit
parseUnit p = coerce $ parse $ spaces >> unit p <* eof
