module PrettyPrinter (showValue) where

import Syntax

showValue :: Value -> String
showValue (Text s) = s
showValue (Number n) = show n
showValue (Decimal d) = show d
showValue (Boolean b) = show b
showValue (Quote exprs) = "'(" ++ unwords (map showExpression exprs) ++ ")"

showExpression :: Expression -> String
showExpression (Constant v) = showValue v
showExpression (Variable name) = name
showExpression (Apply op args) = "(" ++ showOperator op ++ " " ++ unwords (map showExpression args) ++ ")"

showOperator :: Operator -> String
showOperator Plus = "+"
showOperator Minus = "-"
showOperator Times = "*"
showOperator Division = "/"
showOperator LessThan = "<"
showOperator LessThanEqual = "<="
showOperator GreaterThan = ">"
showOperator GreaterThanEqual = ">="
showOperator Equal = "=="
showOperator NotEqual = "/="
showOperator And = "and"
showOperator Or = "or"