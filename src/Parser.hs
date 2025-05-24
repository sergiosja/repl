module Parser (parseProgram) where

import Syntax
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef {
    Token.identStart = letter,
    Token.identLetter = alphaNum,
    Token.reservedOpNames =
        [ "(", ")", "+", "-", "*", "/"
        , "<", ">", "==", "/=", ">=", "<="
        ],
    Token.reservedNames =
        [ "and", "or", "not", "define" ],
    Token.commentStart = "#|",
    Token.commentEnd = "|#",
    Token.commentLine = ";",
    Token.nestedComments = False
}

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

integer :: Parser Integer
integer = Token.integer lexer

decimal :: Parser Double
decimal = Token.float lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer


-- Value

parseValue :: Parser Value
parseValue = choice
    [ try parseText
    , try parseDecimal
    , try parseNumber
    , try parseBoolean
    , parseQuote
    ]

parseText :: Parser Value
parseText = Text <$> stringLiteral

parseDecimal :: Parser Value
parseDecimal = Decimal <$> decimal

parseNumber :: Parser Value
parseNumber = Number <$> integer

parseBoolean :: Parser Value
parseBoolean =
    Boolean <$> (reserved "true" *> pure True <|> reserved "false" *> pure False)

parseQuote :: Parser Value
parseQuote =
    Quote <$> (char '\'' *> char '(' *> parseExpression `sepBy` whiteSpace <* char ')')


-- Statement

parseStatement :: Parser Statement
parseStatement = choice
    [ try parseVariableDeclaration
    ]

parseVariableDeclaration :: Parser Statement
parseVariableDeclaration =
    VariableDeclaration
        <$> (char '(' *> reserved "define" *> identifier)
        <*> (parseValue <* char ')')


-- Expression

parseExpression :: Parser Expression
parseExpression = choice
    [ try parseApply
    , try parseVariable
    , parseConstant
    ]

parseApply :: Parser Expression
parseApply =
    Apply <$> (char '(' *> parseOperator)
          <*> (many parseExpression <* char ')')

parseVariable :: Parser Expression
parseVariable =
    Variable <$> identifier

parseConstant :: Parser Expression
parseConstant = Constant <$> parseValue


-- Operator

parseOperator :: Parser Operator
parseOperator = choice
    [ reservedOp "+" *> pure Plus
    , reservedOp "-" *> pure Minus
    , reservedOp "*" *> pure Times
    , reservedOp "/" *> pure Division
    , reservedOp "<" *> pure LessThan
    , reservedOp ">" *> pure GreaterThan
    , reservedOp "<=" *> pure LessThanEqual
    , reservedOp ">=" *> pure GreaterThanEqual
    , reservedOp "==" *> pure Equal
    , reservedOp "/=" *> pure NotEqual
    , reserved "and" *> pure And
    , reserved "or" *> pure Or
    ]


-- Program

parseProgram :: Parser Program
parseProgram = whiteSpace *>
    (Expression <$> parseExpression <|> Statement <$> parseStatement)