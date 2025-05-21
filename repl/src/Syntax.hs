-- The compiler doesn't let me do (..)
-- :(
module Syntax (module Syntax) where 

data Program = Program [Expression]
    deriving (Show, Eq, Read, Ord)

-- data FunctionDeclaration = Function String [String] [Expression]
--     deriving (Show, Eq, Read, Ord)

data Value =
      Nil
    | Boolean Bool
    | Number Integer
    | Decimal Double
    | Text String
    | List [Value]
    deriving (Show, Eq, Read, Ord)

data Expression =
      Constant Value
    -- | Variable String
    | Parenthesised Expression
    | BinaryOp Operator Expression Expression
    -- | Call String [Expression]
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