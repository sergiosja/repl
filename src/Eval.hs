module Eval (eval) where

import Syntax
import Helpers

eval :: Program -> Either String Value
eval (Program expr) = evalExpression expr


-- Expression

evalExpression :: Expression -> Either String Value
evalExpression (Constant v) = Right v
evalExpression (Apply op args) = do
    values <- traverse evalExpression args
    foldExpression op values


-- Operator

foldExpression :: Operator -> [Value] -> Either String Value
foldExpression Plus values = foldPlus values
foldExpression Minus values = foldMinus values
foldExpression _ _ = Left "Unsupported operator"

foldPlus :: [Value] -> Either String Value
foldPlus [] = Left "Cannot apply operator to empty list"
foldPlus (Number n : rest) =
    if all isNumber rest
    then Right . Number . sum $ map (\(Number x) -> x) (Number n : rest)
    else Left "Expected numbers"
foldPlus (Decimal n : rest) =
    if all isDecimal rest
    then Right . Decimal . sum $ map (\(Decimal x) -> x) (Decimal n : rest)
    else Left "Expected decimals"
foldPlus (Text n : rest) =
    if all isText rest
    then Right . Text . concat $ map (\(Text x) -> x) (Text n : rest)
    else Left "Expected texts"
foldPlus _ = Left "This value doesn't work with plus"

foldMinus :: [Value] -> Either String Value
foldMinus [] = Left "Cannot apply operator to empty list"
foldMinus (Number n : rest) =
    if all isNumber rest
    then Right . Number . foldl (-) n $ map (\(Number x) -> x) rest
    else Left "Expected numbers"
foldMinus (Decimal n : rest) =
    if all isDecimal rest
    then Right . Decimal . foldl (-) n $ map (\(Decimal x) -> x) rest
    else Left "Expected decimals"
foldMinus _ = Left "This value doesn't work with minus"
