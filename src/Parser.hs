module Parser (parseProgram) where

import Syntax
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Data.Functor (($>))
import qualified Text.Parsec.Token as Token

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef {
    Token.identStart = letter,
    Token.identLetter = alphaNum <|> char '?' <|> char '-' <|> char '\'',
    Token.reservedOpNames =
        [ "(", ")", "+", "-", "*", "/"
        , "<", ">", "=", ">=", "<="
        ],
    Token.reservedNames =
        [ "and", "or", "define", "if"
        , "cond", "#t", "#f" ],
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
parseNumber = Number . fromIntegral <$> integer

parseBoolean :: Parser Value
parseBoolean =
    Boolean <$> (reserved "#t" $> True <|> reserved "#f" $> False)

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
    [ try parseIf
    , try parseCond
    , try parseApply
    , try parseCall
    , try parseVariable
    , parseConstant
    ]

parseExpressions :: Parser [Expression]
parseExpressions =
    many parseExpression

parseIf :: Parser Expression
parseIf = parens $
    If <$> (reserved "if" *> parseExpression) <*> parseExpression <*> parseExpression

parseCond :: Parser Expression
parseCond = parens $
    Cond <$> (reserved "cond" *> many1 condPair)
    where
        condPair = parens $ (,) <$> parseExpression <*> parseExpression

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
    [ try $ reservedOp "+" $> Plus
    , try $ reservedOp "-" $> Minus
    , try $ reservedOp "*" $> Times
    , try $ reservedOp "/" $> Division
    , try $ reservedOp "<" $> LessThan
    , try $ reservedOp ">" $> GreaterThan
    , try $ reservedOp "<=" $> LessThanEqual
    , try $ reservedOp ">=" $> GreaterThanEqual
    , try $ reservedOp "=" $> Equal
    , try $ reservedOp "and" $> And
    , reservedOp "or" $> Or
    ]


-- Program

parseProgram :: Parser Program
parseProgram = whiteSpace *>
    (try (Expression <$> parseExpression) <|> Statement <$> parseStatement)
    <* many (char ')')