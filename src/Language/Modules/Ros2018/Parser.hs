{-# LANGUAGE OverloadedStrings #-}

module Language.Modules.Ros2018.Parser
  ( parseText
  , SyntaxError
  ) where

import Data.Coerce
import qualified Data.Text as T
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import Language.Modules.Ros2018
import Language.Modules.Ros2018.Display
import Language.Modules.Ros2018.Internal (Literal(..))
import qualified Language.Modules.Ros2018.Internal as I
import Language.Modules.Ros2018.Position

newtype SyntaxError = SyntaxError (ParseErrorBundle T.Text Void)
  deriving (Eq, Show)

instance Display SyntaxError where
  display (SyntaxError eb) = errorBundlePretty eb

parseText :: FilePath -> T.Text -> Either SyntaxError (Positional Expr)
parseText fp xs = coerce $ parse whileParser fp xs

type Parser = Parsec Void T.Text

-- Space consumer.
sc :: Parser ()
sc = L.space space1 line block
  where
    line  = L.skipLineComment "--"
    block = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser (Positional a)
lexeme p = L.lexeme sc $ do
  start <- getSourcePos
  x <- p
  end <- getSourcePos
  return $ positional (position start end) x

symbol :: T.Text -> Parser (Positional T.Text)
symbol = lexeme . C.string

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser (Positional Int)
integer = lexeme L.decimal

bool :: Parser (Positional Bool)
bool = flip positional True <$> reserved "true"
   <|> flip positional False <$> reserved "false"

character :: Parser (Positional Char)
character = lexeme $ between (char '\'') (char '\'') anySingle

literal :: Parser (Positional Literal)
literal = foldl (<|>) empty
  [ fmap LInt <$> integer
  , fmap LBool <$> bool
  , fmap LChar <$> character
  ]

-- Reserved words, that is, keywords.
reserved :: T.Text -> Parser Position
reserved w = fmap getPosition $ lexeme $ try $ string w *> notFollowedBy alphaNumChar

reservedWords :: [T.Text]
reservedWords =
  [ "true"
  , "false"
  , "struct"
  , "end"
  , "include"
  ]

identifier :: Parser (Positional Ident)
identifier = lexeme $ try $ p >>= check
  where
    p = fmap T.pack $ (:) <$> letterChar <*> many alphaNumChar
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " is not an identifier"
        else return $ ident x

bindings :: Parser [Positional Binding]
bindings = binding `sepEndBy` symbol ";"

structure :: Parser (Positional Expr)
structure = do
  start <- reserved "struct"
  bs <- bindings
  end <- reserved "end"
  return $ positional (connect start end) $ Struct bs

expression :: Parser (Positional Expr)
expression = foldl (<|>) empty
  [ fmap Lit <$> literal
  , fmap Id <$> identifier
  , structure
  ]

binding :: Parser (Positional Binding)
binding = foldl (<|>) empty
  [ (\id e -> positional (getPosition id `connect` getPosition e) $ Val (fromPositional id) e) <$> identifier <*> (symbol "=" >> expression)
  , (\pos e -> positional pos $ Include e) <$> reserved "include" <*> expression
  ]

whileParser :: Parser (Positional Expr)
whileParser = between sc eof $ f <$> bindings
  where
    f :: [Positional Binding] -> Positional Expr
    f []       = positional dummyPos $ Struct []
    f (b : bs) =
      let start = getPosition b in
      let end = lastPosition b bs in
        positional (connect start end) $ Struct $ b : bs

lastPosition :: Positional a -> [Positional b] -> Position
lastPosition x [] = getPosition x
lastPosition _ xs = getPosition $ last xs
