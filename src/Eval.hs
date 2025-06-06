module Eval (Scope(..), REPL, run) where

import Syntax
import Helpers

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad (zipWithM_)

type Procedure = ([String], Expression)
type Procedures = Map.Map String Procedure
type Stack = [[(String, Value)]]

data Scope = Scope
  { procedures :: Procedures, stack :: Stack}
  deriving (Show, Eq)
type REPL a = StateT Scope IO a

run :: Program -> Scope -> IO (Either String Value, Scope)
run = runStateT . eval

eval :: Program -> REPL (Either String Value)
eval (Expression expr) = evalExpression expr
eval (Statement stmt) = evalStatement stmt


-- Scope management

pushScope :: REPL ()
pushScope = do
  scope@(Scope { stack = stack' }) <- get
  put scope { stack = [] : stack' }

popScope :: REPL ()
popScope = do
  scope@(Scope { stack = (_ : rest) }) <- get
  put scope { stack = rest }

bindVar :: String -> Value -> REPL ()
bindVar name value = do
  scope@(Scope { stack = (current : rest) }) <- get
  put scope { stack = ((name, value) : current) : rest }


-- Statement

evalStatement :: Statement -> REPL (Either String Value)
evalStatement (VariableDeclaration name expression) = do
  maybeExpression <- evalExpression expression
  case maybeExpression of
    Left err -> return $ Left err
    Right value -> do
      bindVar name value
      return $ Right $ Text ("#<var:" ++ name ++ ">")
evalStatement (ProcedureDeclaration name args body) = do
  scope@(Scope { procedures = procedures' }) <- get
  put scope { procedures = Map.insert name (args, body) procedures' }
  return $ Right $ Text ("#<procedure:" ++ name ++ ">")


-- Expression

evalExpression :: Expression -> REPL (Either String Value)
evalExpression (Constant v) = return $ Right v
evalExpression (Variable name) = do
  Scope { stack = stack' } <- get
  return $ searchScopes stack' name
  where
    searchScopes [] _ = Left $ "Variable not found: " ++ name
    searchScopes (current : rest) var =
      case lookup var current of
        Just value -> Right value
        Nothing -> searchScopes rest var
evalExpression (Call name args) = do
  maybeProc <- searchScopes name
  case maybeProc of
    Just (params, expression) ->
        if length params /= length args then
          return $ Left ("Expexted " ++ show (length params) ++ " arguments, but got " ++ show (length args))
        else do
          maybeValues <- mapM evalExpression args
          case sequence maybeValues of
            Left err -> return $ Left err
            Right values -> do
              pushScope
              zipWithM_ bindVar params values
              evalExpression expression <* popScope
    Nothing -> return $ Left $ "Procedure not found: " ++ name
  where
    searchScopes :: String -> REPL (Maybe Procedure)
    searchScopes name' = do
      Scope { procedures = env } <- get
      return $ Map.lookup name' env
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
