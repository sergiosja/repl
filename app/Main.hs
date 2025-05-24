module Main (main) where

import System.IO (hFlush, stdout)
import Data.List(isPrefixOf)
import Text.Parsec

import PrettyPrinter (showValue)
import Parser (parseProgram)
import Eval (run)


main :: IO ()
main = do
    putStrLn "repl, version 0.0.1: https://github.com/sergiosja/repl  :? for help (not implemented)"
    repl []
    where
        repl stack = do
            putStr "\nь > "
            hFlush stdout
            line <- getLine
            case line of
                "ciao" -> do
                    putStrLn "Arrivederci caro 👋"
                    return ()
                _ | isPrefixOf "ast" line -> do
                    printAST $ drop 3 line
                    repl stack
                _ ->
                    case parse parseProgram "" line of
                        Right p -> do
                            (res, newStack) <- run p stack
                            case res of
                                Left err ->
                                    continue ("Eval error: " ++ show err) stack
                                Right value -> putStrLn $ showValue value
                            repl newStack
                        Left err ->
                            continue ("Parse error: " ++ show err) stack
        continue msg stack = do
            putStrLn $ msg
            repl stack


printAST :: String -> IO ()
printAST program = do
    case parse parseProgram "" program of
        Right ast -> print ast
        Left err -> putStrLn $ "Parse error when printing AST: " ++ show err