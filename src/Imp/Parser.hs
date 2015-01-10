module Imp.Parser where

import Control.Applicative ((<$>), (<*>), (<$), (<*), (*>), (<|>))
import Data.Text (Text)
import Text.Parser.Expression
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Text.Parsec (parse, ParseError)
import Text.Parsec.Text (Parser)

import Imp.Syntax

opTable :: OperatorTable Parser Expr
opTable =
    [ [ Postfix (flip Apply <$> parens (commaSep exprParser)) ]
    , [ op Mul "*" AssocLeft, op Div "/" AssocLeft ]
    , [ op Add "+" AssocLeft, op Sub "-" AssocLeft ]
    , [ op App "++" AssocRight ]
    , [ op Eq "==" AssocNone, op Neq "!=" AssocNone
      , op Lt "<" AssocNone, op Le "<=" AssocNone
      , op Gt ">" AssocNone, op Ge ">=" AssocNone
      ]
    , [ op And "&&" AssocNone ]
    , [ op Or "||" AssocNone ]
    ]
  where
    op constr str assoc = Infix (Op constr <$ symbol str) assoc

opParser :: Parser Expr
opParser = buildExpressionParser opTable $
           try funcParser <|>
           parens opParser <|>
           Const <$> try constParser <|>
           Id <$> identifierParser

exprParser :: Parser Expr
exprParser = try funcParser <|>
             try opParser <|>
             Const <$> try constParser <|>
             Id <$> identifierParser

constParser :: Parser Const
constParser = Number <$> number <|>
              Boolean <$> boolean <|>
              String <$> stringLiteral
  where
    number = toDouble <$> integerOrDouble
    toDouble (Left n) = fromInteger n
    toDouble (Right n) = n
    boolean = True <$ symbol "true" <|>
              False <$ symbol "false"

identifierParser :: Parser Id
identifierParser = MkId <$> ((:) <$> idHead <*> idTail) <* whiteSpace
  where
    idHead = lower <|> char '_'
    idTail = many (alphaNum <|> oneOf "_'!?")

funcParser :: Parser Expr
funcParser = Fun <$> (symbol "fun" *> args) <*> braces impParser
  where
    args = parens (commaSep identifierParser)

statementParser :: Parser Statement
statementParser = varParser <|>
                  ifParser <|>
                  whileParser <|>
                  returnParser <|>
                  try assignParser <|>
                  Expr <$> exprParser <* symbol ";"
  where
    varParser = Var <$>
                (symbol "var" *> identifierParser <* symbol "=") <*>
                (exprParser <* symbol ";")
    assignParser = Assign <$> identifierParser <* symbol "=" <*> exprParser <* symbol ";"
    ifParser = If <$>
               (symbol "if" *> parens exprParser) <*>
               braces impParser <*>
               (symbol "else" *> braces impParser)
    whileParser = While <$>
                  (symbol "while" *> parens exprParser) <*>
                  braces impParser
    returnParser = Return <$> (symbol "return" *> exprParser <* symbol ";")

impParser :: Parser Program
impParser = whiteSpace *> many statementParser

parseCode :: Text -> Either ParseError Program
parseCode = parse (impParser <* eof) "stdin"
