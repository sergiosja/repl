module Eval (run) where

import Syntax
import Helpers
import PrettyPrinter (showValue)

import Control.Monad.State

type Stack = [(String, Value)]
type REPL a = StateT Stack IO a


run :: Program -> Stack -> IO (Either String Value, Stack)
run = runStateT . eval

eval :: Program -> REPL (Either String Value)
eval (Expression expr) = evalExpression expr
eval (Statement stmt) = evalStatement stmt


-- Statement

evalStatement :: Statement -> REPL (Either String Value)
evalStatement (VariableDeclaration name value) = do
    modify ((name, value):)
    return $ Right $ Text ("#<var:" ++ name ++ "=" ++ showValue value ++ ">")



-- Expression

evalExpression :: Expression -> REPL (Either String Value)
evalExpression (Constant v) = return $ Right v
evalExpression (Variable name) = do
    stack <- get
    return $
        case lookup name stack of
            Just value -> Right value
            Nothing -> Left $ "Variable not found: " ++ name
evalExpression (Apply op args) = do
    values <- traverse evalExpression args
    case sequence values of
        Left err -> return $ Left err
        Right values' -> return $ foldExpression op values'


-- Operator

foldExpression :: Operator -> [Value] -> Either String Value
foldExpression Plus values = foldPlus values
foldExpression Minus values = foldMinus values
foldExpression Times values = foldTimes values
foldExpression Division values = foldDivision values
foldExpression LessThan values = foldComparison (<) values
foldExpression LessThanEqual values = foldComparison (<=) values
foldExpression GreaterThan values = foldComparison (>) values
foldExpression GreaterThanEqual values = foldComparison (>=) values
foldExpression Equal values = foldComparison (==) values
foldExpression NotEqual values = foldComparison (/=) values
-- foldExpression And values = foldComparison (&&) values
-- foldExpression Or values = foldComparison (||) values
foldExpression _ _ = Left "Unsupported operator"

foldPlus :: [Value] -> Either String Value
foldPlus values
  | all isNumber values = foldNumbers (+) values
  | all isDecimal values = foldDecimals (+) values
  | all isText values = foldTexts (++) values
  | otherwise = Left "foldPlus fail"

foldMinus :: [Value] -> Either String Value
foldMinus values
  | all isNumber values = foldNumbers (-) values
  | all isDecimal values = foldDecimals (-) values
  | otherwise = Left "foldMinus fail"

foldTimes :: [Value] -> Either String Value
foldTimes values
  | all isNumber values = foldNumbers (*) values
  | all isDecimal values = foldDecimals (*) values
  | otherwise = Left "foldTimes fail"

foldDivision :: [Value] -> Either String Value
foldDivision values
  | all isNumber values = foldNumbers div values
  | all isDecimal values = foldDecimals (/) values
  | otherwise = Left "foldDivision fail"

foldComparison :: (Value -> Value -> Bool) -> [Value] -> Either String Value
foldComparison _ [] = Right $ Boolean True
foldComparison _ [_] = Right $ Boolean True
foldComparison f (x:y:xs)
    | f x y = foldComparison f (y:xs)
    | otherwise = Right $ Boolean False
