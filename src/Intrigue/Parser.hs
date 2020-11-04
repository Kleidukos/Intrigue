module Intrigue.Parser where

import           Control.Monad.Combinators.Expr
import qualified Data.Text                      as T
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Vector as V

import Prelude hiding (many)
import Intrigue.Types
import Intrigue.Lexer

parseAtom :: Parser AST
parseAtom = do
  let atomHead = oneOf ("!$€%&|*+-/:<=>?@^_~" :: [Token Text])
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
  let identifiers = oneOf ("()!$€%&|*+-/:<=>?@^_~" :: [Token Text])
                    <|> letterChar
                    <|> digitChar
  x <- (string "space" >> pure ' ')
   <|> (string "newline" >> pure '\n')
   <|> identifiers
  pure $ Character x

parseNumber :: Parser AST
parseNumber = Number <$> integer

parseNegNumber :: Parser AST
parseNegNumber = Number <$> signedInteger

parseSExp :: Parser AST
parseSExp =
  List . V.fromList . concat <$> parens (many parseExp `sepBy` (char ' ' <|> char '\n'))

parseQuote :: Parser AST
parseQuote = do
  char '\''
  x <- parseExp
  pure $ Quote x

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
