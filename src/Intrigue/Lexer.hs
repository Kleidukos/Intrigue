module Intrigue.Lexer where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

skipLineComment :: Parser ()
skipLineComment = L.skipLineComment ";"

skipBlockComment :: Parser ()
skipBlockComment = L.skipBlockComment "{-" "-}"

consumeSpaces :: Parser ()
consumeSpaces = L.space space1 skipLineComment skipBlockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme consumeSpaces

symbol :: Text -> Parser Text
symbol = L.symbol consumeSpaces

integer :: Parser Integer
integer = lexeme L.decimal

signedInteger :: Parser Integer
signedInteger = L.signed consumeSpaces integer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
