-- The compiler doesn't let me do (..)
-- :(
module Syntax (module Syntax) where 

data Program = Expression Expression | Statement Statement
  deriving (Show, Eq, Read, Ord)

data Statement =
    VariableDeclaration String Expression
  | ProcedureDeclaration String [String] Expression
  deriving (Show, Eq, Read, Ord)

data Expression =
    Constant Value
  | Variable String
  | If Expression Expression Expression
  | Cond [(Expression, Expression)]
  | Apply Operator [Expression]
  | Call String [Expression]
  deriving (Show, Eq, Read, Ord)

data Value =
    Text String
  | Number Int
  | Decimal Double
  | Boolean Bool
  | Quote [Expression]
  | Void
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
  | And
  | Or
  deriving (Show, Eq, Read, Ord)