-- The compiler doesn't let me do (..)
-- :(
module Syntax (module Syntax) where 

data Program = Program [Expression]
    deriving (Show, Eq, Read, Ord)

data Value =
      Text String
    | Number Integer
    | Decimal Double
    | Boolean Bool
    | List [Value]
    deriving (Show, Eq, Read, Ord)

data Expression =
      Constant Value
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