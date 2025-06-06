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

wrapWS :: Parser a -> Parser a
wrapWS p = whiteSpace *> p <* whiteSpace

parens :: Parser a -> Parser a
parens = Token.parens lexer

-- Value

parseValue :: Parser Value
parseValue = wrapWS $ choice
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
parseStatement = wrapWS $ choice
    [ try parseVariableDeclaration
    , parseProcedureDeclaration
    ]

parseVariableDeclaration :: Parser Statement
parseVariableDeclaration = parens $
    VariableDeclaration
        <$> (reserved "define" *> identifier) <*> parseExpression

parseProcedureDeclaration :: Parser Statement
parseProcedureDeclaration = parens $
    ProcedureDeclaration
        <$> (reserved "define" *> char '(' *> identifier)
        <*> (many1 identifier <* char ')')
        <*> parseExpression

-- Expression

parseExpression :: Parser Expression
parseExpression = wrapWS $ choice
    [ try parseApply
    , try parseCall
    , try parseVariable
    , parseConstant
    ]

parseExpressions :: Parser [Expression]
parseExpressions =
    many parseExpression

parseApply :: Parser Expression
parseApply = parens $
    Apply <$> parseOperator <*> parseExpressions

parseCall :: Parser Expression
parseCall = parens $
    Call <$> (whiteSpace *> identifier)
         <*> parseExpressions

parseVariable :: Parser Expression
parseVariable = Variable <$> identifier

parseConstant :: Parser Expression
parseConstant = Constant <$> parseValue


-- Operator

parseOperator :: Parser Operator
parseOperator = wrapWS $ choice
    [ try $ reservedOp "+" *> pure Plus
    , try $ reservedOp "-" *> pure Minus
    , try $ reservedOp "*" *> pure Times
    , try $ reservedOp "/" *> pure Division
    , try $ reservedOp "<" *> pure LessThan
    , try $ reservedOp ">" *> pure GreaterThan
    , try $ reservedOp "<=" *> pure LessThanEqual
    , try $ reservedOp ">=" *> pure GreaterThanEqual
    , try $ reservedOp "==" *> pure Equal
    , try $ reservedOp "/=" *> pure NotEqual
    , try $ reservedOp "and" *> pure And
    , reservedOp "or" *> pure Or
    ]


-- Program

parseProgram :: Parser Program
parseProgram = whiteSpace *>
    (try (Expression <$> parseExpression) <|> Statement <$> parseStatement)
    <* (many $ char ')')