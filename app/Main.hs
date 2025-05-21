module Main (main) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Control.Monad (forever)
import Parser (parseProgram)
import Text.Parsec


main :: IO ()
main = do
    putStrLn "repl, version 0.0.1: https://github.com/sergiosja/repl  :? for help (not implemented)\n"
    repl

repl :: IO ()
repl = forever $ do
    putStr "σ "
    hFlush stdout
    line <- getLine
    case parse parseProgram "" line of
        Right p -> do
            print p
            putStrLn ""
        Left err -> putStrLn $ "Parse error: " ++ show err

printAST :: String -> IO ()
printAST program = do
    case parse parseProgram "" program of
        Right ast -> print ast
        Left err -> putStrLn $ "Parse error when printing AST: " ++ show err