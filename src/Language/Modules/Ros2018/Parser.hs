{-# LANGUAGE OverloadedStrings #-}

module Language.Modules.Ros2018.Parser
  ( parseText
  , SyntaxError
  ) where

import Data.Coerce
import Data.Functor
import qualified Data.Text as T
import Data.Void
import GHC.Exts

import Control.Monad.Combinators.Expr
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

baseType :: Parser (Positional I.BaseType)
baseType = foldl (<|>) empty
  [ (`positional` I.Bool) <$> reserved "bool"
  , (`positional` I.Int) <$> reserved "int"
  , (`positional` I.Char) <$> reserved "char"
  ]

typeParser :: Parser (Positional Type)
typeParser = makeExprParser withParam typeOpTable >>= f
  where
    f :: PType -> Parser (Positional Type)
    f (Typ ty) = return ty
    f _        = unexpected $ Label $ fromList "(arrow | where)-related parse error"

data PType
  = Typ (Positional Type)
  | WithParam Ident (Positional Type)
  | WhereR [Ident] (Positional Type)
  | Fail

withParam :: Parser PType
withParam = foldl (<|>) empty
  [ try $ parens $ WithParam <$> (fromPositional <$> identifier) <*> (symbol ":" >> typeParser)
  , try $ parens $ WhereR <$> (map fromPositional <$> identifier `sepBy` char '.') <*> (symbol ":" >> typeParser)
  , Typ <$> typeAtom
  ]

typeAtom :: Parser (Positional Type)
typeAtom = foldl (<|>) empty
  [ fmap Base <$> baseType
  , (`positional` TypeType) <$> reserved "type"
  , try $ (\p ty -> positional p $ WrapType ty) <$> reserved "wrap" <*> typeParser
  , try $ parens $ (\eq e -> connecting eq e $ Singleton e) <$> symbol "=" <*> expression
  , parens typeParser
  , try $ fmap Expr <$> expression
  , signature
  ]

arrow :: Purity -> PType -> PType -> PType
arrow p (Typ ty1) (Typ ty2)          = Typ $ connecting ty1 ty2 $ Arrow Nothing ty1 p ty2
arrow p (WithParam id ty1) (Typ ty2) = Typ $ connecting ty1 ty2 $ Arrow (Just id) ty1 p ty2
arrow _ _ _                          = Fail

where_ :: PType -> PType -> PType
where_ (Typ ty1) (WithParam id ty2) = Typ $ connecting ty1 ty2 $ Where ty1 [id] ty2
where_ (Typ ty1) (WhereR ids ty2)   = Typ $ connecting ty1 ty2 $ Where ty1 ids ty2
where_ _ _                          = Fail

typeOpTable :: [[Operator Parser PType]]
typeOpTable =
  [ [ InfixL $ (\ty1 ty2 -> where_ ty1 ty2) <$ reserved "where"
    ]
  , [ InfixR $ (\ty1 ty2 -> arrow Pure ty1 ty2) <$ symbol "->"
    , InfixR $ (\ty1 ty2 -> arrow Impure ty1 ty2) <$ symbol "~>"
    ]
  ]

-- Reserved words, that is, keywords.
reserved :: T.Text -> Parser Position
reserved w = fmap getPosition $ lexeme $ try $ string w *> notFollowedBy alphaNumChar

reservedWords :: [T.Text]
reservedWords =
  [ "true"
  , "false"
  , "struct"
  , "sig"
  , "end"
  , "include"
  , "where"
  , "bool"
  , "int"
  , "char"
  , "type"
  , "fun"
  , "if"
  , "then"
  , "else"
  , "wrap"
  , "unwrap"
  , "let"
  , "in"
  , "local"
  ]

identifier :: Parser (Positional Ident)
identifier = lexeme $ try $ p >>= check
  where
    p = fmap T.pack $ (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
    check x =
      if x `elem` reservedWords
        then fail $ "keyword " ++ show x ++ " is not an identifier"
        else return $ ident x

decl :: Parser (Positional Decl)
decl = foldl (<|>) empty
  [ (\id ty -> connecting id ty $ Spec (fromPositional id) ty) <$> identifier <*> (symbol ":" >> typeParser)
  , (\pos ty -> positional (connect pos $ getPosition ty) $ DInclude ty) <$> reserved "include" <*> typeParser
  ]

decls :: Parser [Positional Decl]
decls = decl `sepEndBy` symbol ";"

signature :: Parser (Positional Type)
signature = do
  start <- reserved "sig"
  ds <- decls
  end <- reserved "end"
  return $ positional (connect start end) $ Sig ds

bindings :: Parser [Positional Binding]
bindings = binding `sepEndBy` symbol ";"

structure :: Parser (Positional Expr)
structure = do
  start <- reserved "struct"
  bs <- bindings
  end <- reserved "end"
  return $ positional (connect start end) $ Struct bs

seal :: Parser ()
seal = void $ symbol ":>"

expression :: Parser (Positional Expr)
expression = do
  e <- expression'
  ids <- many $ char '.' >> identifier
  return $ foldl (\e id -> connecting e id $ Proj e $ fromPositional id) e ids

expression' :: Parser (Positional Expr)
expression' = foldl (<|>) empty
  [ fmap Lit <$> literal
  , (\p id ty -> positional (connect p $ getPosition ty) $ Wrap id ty) <$> reserved "wrap" <*> identifier <*> (symbol ":" >> typeParser)
  , (\p id ty -> positional (connect p $ getPosition ty) $ Unwrap id ty) <$> reserved "unwrap" <*> identifier <*> (symbol ":" >> typeParser)
  , try $ (\id ty -> connecting id ty $ Seal id ty) <$> identifier <*> (seal >> typeParser)
  , try $ (\id1 id2 -> connecting id1 id2 $ App id1 id2) <$> identifier <*> identifier
  , fmap Id <$> identifier
  , structure
  , (\p ty -> positional p $ Type ty) <$> reserved "type" <*> typeParser
  , (\p param e -> positional (connect p $ getPosition e) $ Abs param e) <$> reserved "fun" <*> params <*> (symbol "=>" >> expression)
  , (\p id e1 e2 ty -> positional (connect p $ getPosition ty) $ If id e1 e2 ty) <$> reserved "if" <*> identifier <*> (reserved "then" >> expression) <*> (reserved "else" >> expression) <*> (reserved "end" >> symbol ":" >> typeParser)
  , (\p bs e -> positional (connect p $ getPosition e) $ Let bs e) <$> reserved "let" <*> bindings <*> (reserved "in" >> expression)
  ]

params :: Parser Param
params = foldl (<|>) empty
  [ parens (Param <$> identifier <*> (symbol ":" >> typeParser))
  , Omit <$> identifier
  ]

binding :: Parser (Positional Binding)
binding = foldl (<|>) empty
  [ (\id e -> positional (getPosition id `connect` getPosition e) $ Val (fromPositional id) e) <$> identifier <*> (symbol "=" >> expression)
  , (\pos e -> positional (connect pos $ getPosition e) $ Include e) <$> reserved "include" <*> expression
  , (\p1 bs1 bs2 p2 -> positional (connect p1 p2) $ Local bs1 bs2) <$> reserved "local" <*> bindings <*> (reserved "in" >> bindings) <*> reserved "end"
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
