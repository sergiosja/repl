module Main (main) where

import System.IO (hFlush, stdout)
import Data.List(isPrefixOf)
import Text.Parsec

import PrettyPrinter (showValue)
import Parser (parseProgram)
import Helpers (isBalanced)
import Eval (run)


main :: IO ()
main = do
    putStrLn "repl, version 0.0.1: https://github.com/sergiosja/repl  :? for help (not implemented)"
    repl []
    where
        repl stack = do
            putStr "\nь > "
            hFlush stdout
            input <- readMultiline []
            case unwords input of
                program | isPrefixOf "ast" program -> do
                    printAST $ drop 3 program
                    repl stack

                program | isPrefixOf "ciao" program -> do
                    putStrLn "Arrivederci caro 👋"
                    return ()

                program ->
                    case parse parseProgram "" program of
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

readMultiline :: [String] -> IO [String]
readMultiline acc = do
    line <- getLine
    let program = acc ++ [line]
    case () of
        _ | isPrefixOf "ciao" line -> return ["ciao"]
          | isBalanced $ unwords program -> return program
          | otherwise -> readMultiline program

printAST :: String -> IO ()
printAST program = do
    case parse parseProgram "" program of
        Right ast -> print ast
        Left err -> putStrLn $ "Parse error when printing AST: " ++ show err

{-
 - Make the program crash nicely if the user inputs too many parentheses, e.g. '())
-}