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

infixTable :: OperatorTable Parser Expr
infixTable =
    [ [ op Mul "*" AssocLeft, op Div "/" AssocLeft ]
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

infixParser :: Parser Expr
infixParser = buildExpressionParser infixTable $
              parens infixParser

exprParser :: Parser Expr
exprParser = Const <$> constParser <|>
             funcParser <|>
             infixParser <|>
             applyParser <|>
             Id <$> identifierParser

constParser :: Parser Const
constParser = Number <$> double <|>
              Boolean <$> boolean <|>
              String <$> stringLiteral
  where
    boolean = True <$ symbol "true" <|> False <$ symbol "false"

identifierParser :: Parser Id
identifierParser = MkId <$> ((:) <$> idHead <*> idTail)
  where
    idHead = lower <|> char '_'
    idTail = many (alphaNum <|> oneOf "_'!?")

funcParser :: Parser Expr
funcParser = Fun <$> (symbol "fun" *> args) <*> braces impParser
  where
    args = parens (commaSep identifierParser)

applyParser :: Parser Expr
applyParser = Apply <$> exprParser <*> parens (commaSep exprParser)

statementParser :: Parser Statement
statementParser = varParser <|>
                  ifParser <|>
                  whileParser <|>
                  returnParser <|>
                  printParser <|>
                  assignParser <|>
                  Expr <$> exprParser
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
    printParser = Print <$> (symbol "print" *> exprParser <* symbol ";")

impParser :: Parser [Statement]
impParser = many statementParser

parseCode :: Text -> Either ParseError Program
parseCode = parse impParser "stdin"
