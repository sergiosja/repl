module Parser (parseProgram) where

import Syntax
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef {
    -- Token.identStart = letter,
    -- Token.identLetter = alphaNum <|> char '\'',
    Token.reservedOpNames =
        [ "(", ")", "+", "-", "*", "/", "\'"
        , "<", ">", "==", "/=", ">=", "<="
        ],
    Token.reservedNames =
        [ "and", "or", "not" ],
    Token.commentStart = "#|",
    Token.commentEnd = "|#",
    Token.commentLine = ";",
    Token.nestedComments = False
}

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- whiteSpace' :: Char -> Parser ()
-- whiteSpace' c = whiteSpace >> char c >> whiteSpace

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
    , parseList
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

parseList :: Parser Value
parseList =
    List <$> (reserved "'" *> reserved "(" *> parseExpression `sepBy` whiteSpace <* reserved ")")


-- Expression

parseExpression :: Parser Expression
parseExpression = choice
    [ try parseApply
    , parseConstant
    ]

parseApply :: Parser Expression
parseApply =
    Apply <$> (reserved "(" *> parseOperator)
          <*> (many parseExpression <* reserved ")")

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
parseProgram =
    Program <$> parseExpression