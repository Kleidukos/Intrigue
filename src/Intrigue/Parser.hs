module Intrigue.Parser where

import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text                      as T
import qualified Data.Vector as V
import qualified Text.Megaparsec.Char.Lexer as L

import Intrigue.Types
import Intrigue.Lexer

nonAlphaNumTokens :: [Token Text]
nonAlphaNumTokens = "!?¡¿$€%&|*×÷+-/:<=>@^_~"

parseAtom :: Parser AST
parseAtom = do
  let atomHead = oneOf nonAlphaNumTokens
  beginning <- letterChar <|> atomHead
  rest  <- many (letterChar <|> digitChar <|> atomHead)
  let atom = [beginning] <> rest
  pure $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom $ T.pack atom

parseText :: Parser AST
parseText = do
  char '"'
  x <- manyTill L.charLiteral (char '"')
  pure $ String $ T.pack x

parseCharacter :: Parser AST
parseCharacter = do
  chunk "#\\"
  x <- (string "space" >> pure ' ')
   <|> (string "newline" >> pure '\n')
   <|> printChar
  pure $ Character x

parseNumber :: Parser AST
parseNumber = Number <$> integer

parseNegNumber :: Parser AST
parseNegNumber = do
  char '-'
  d <- integer
  pure . Number . negate $ d

parseSExp :: Parser AST
parseSExp =
  List . ASTList . V.fromList . concat <$> parens (many parseExp `sepBy` (char ' ' <|> char '\n'))

parseQuote :: Parser AST
parseQuote = do
  char '\''
  Quote <$> parseExp

parseNil :: Parser AST
parseNil = do
  chunk "Nil"
  pure Nil

parseTerm :: Parser AST
parseTerm = (parseNil <?> "Nil")
        <|> (parseNumber <?> "Number")
        <|> try (parseNegNumber <?> "negative Number")
        <|> (parseAtom <?> "Atom")
        <|> (parseText <?> "Text")
        <|> (parseCharacter <?> "Character")
        <|> (parseQuote <?> "Quote")
        <|> (parseSExp <?> "S-Expression")

parseExp :: Parser AST
parseExp = makeExprParser parseTerm [] <?> "Expression"
