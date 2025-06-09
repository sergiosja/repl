module PrettyPrinter (showValue) where

import Syntax

showValue :: Value -> String
showValue (Text s) = s
showValue (Number n) = show n
showValue (Decimal d) = show d
showValue (Boolean b) = show b
showValue (Quote exprs) = "'(" ++ unwords (map showExpression exprs) ++ ")"
showValue Void = "#<void:>"

showExpression :: Expression -> String
showExpression (Constant v) = showValue v
showExpression (Variable name) = name
showExpression (Call name args) = "(" ++ name ++ " " ++ unwords (map showExpression args) ++ ")"
showExpression (Apply op args) = "(" ++ showOperator op ++ " " ++ unwords (map showExpression args) ++ ")"
showExpression (If cond case1 case2) = "(if " ++ showExpression cond ++ showExpression case1 ++ showExpression case2 ++ ")"
showExpression (Cond branches) = "(cond (" ++ concatMap showBranch branches ++ "))"
    where showBranch (x, y) = "(" ++ showExpression x ++ showExpression y ++ ")"

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