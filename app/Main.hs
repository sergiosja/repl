module Main (main) where

import System.IO (hFlush, stdout)
import Control.Monad (forever)
import Text.Parsec

import Parser (parseProgram)
import Eval (eval)

main :: IO ()
main = repl

repl :: IO ()
repl = do
    putStrLn "repl, version 0.0.1: https://github.com/sergiosja/repl  :? for help (not implemented)"
    loop
    where
        loop = do
            putStr "\nσ "
            hFlush stdout
            line <- getLine
            case line of
                "ciao" -> return ()
                _ -> do
                    case parse parseProgram "" line of
                        Right p ->
                            case eval p of
                                Right result -> putStrLn $ show result
                                Left err -> putStrLn $ "Eval error: " ++ show err
                        Left err -> putStrLn $ "Parse error: " ++ show err
                    loop

-- printAST :: String -> IO ()
-- printAST program = do
--     case parse parseProgram "" program of
--         Right ast -> print ast
--         Left err -> putStrLn $ "Parse error when printing AST: " ++ show err