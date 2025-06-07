module Main (main) where

import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Data.List(isPrefixOf)
import Text.Parsec

import PrettyPrinter (showValue)
import Parser (parseProgram)
-- import Helpers (isBalanced)
import Eval (Scope(..), run)
import Syntax

initialScope :: Scope
initialScope = Scope
    { procedures = Map.empty
    , stack = [[]]
    }

main :: IO ()
main = do
    putStrLn "repl, version 0.0.1: https://github.com/sergiosja/repl  :? for help (not implemented)"
    repl initialScope
    where
        repl scope = do
            putStr "\nь > "
            hFlush stdout
            program <- getLine
            if program == "ciao"
                then do
                    putStrLn "Arrivederci caro 👋"
                    return ()
            else if isPrefixOf "ast" program
                then do
                    printAST $ drop 3 program
                    repl scope
            else do
                res <- evalIfParsed program scope
                case res of
                    Left errorMessage ->
                        continue errorMessage scope
                    Right (value, newScope) ->
                        continue (showValue value) newScope
        continue msg scope = do
            putStrLn $ msg
            repl scope

evalIfParsed :: String -> Scope -> IO (Either String (Value, Scope))
evalIfParsed program scope =
    case parse parseProgram "" program of
        Left err -> return $ Left $ "Parse error: " ++ show err
        Right p -> eval p scope

eval :: Program -> Scope -> IO (Either String (Value, Scope))
eval program scope = do
    (res, newScope) <- run program scope
    return $ case res of
        Right value -> Right (value, newScope)
        Left err -> Left $ "Eval error: " ++ show err

printAST :: String -> IO ()
printAST program = do
    case parse parseProgram "" program of
        Right ast -> print ast
        Left err -> putStrLn $ "Parse error when printing AST: " ++ show err
