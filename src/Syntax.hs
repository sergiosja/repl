-- The compiler doesn't let me do (..)
-- :(
module Syntax (module Syntax) where 

data Program = Expression Expression | Statement Statement
    deriving (Show, Eq, Read, Ord)

data Value =
      Text String
    | Number Integer
    | Decimal Double
    | Boolean Bool
    | Quote [Expression]
    deriving (Show, Eq, Read, Ord)

data Statement = VariableDeclaration String Value -- også kommer funksjoner
    deriving (Show, Eq, Read, Ord)

data Expression =
      Constant Value
    | Variable String
    | Apply Operator [Expression]
    deriving (Show, Eq, Read, Ord)

data Operator =
      Plus
    | Minus
    | Times
    | Division
    | LessThan
    | LessThanEqual
    | GreaterThan
    | GreaterThanEqual
    | Equal
    | NotEqual
    | And
    | Or
    deriving (Show, Eq, Read, Ord)