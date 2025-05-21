module Main (main) where

import System.Environment (getArgs)
import Text.Parsec
import Parser (parseProgram)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [program] -> printAST program
        _ -> putStrLn "Scusami ma, non ho capito. Provi di nuovo, per favore."


printAST :: String -> IO ()
printAST program = do
    case parse parseProgram "" program of
        Right ast -> print ast
        Left err -> putStrLn $ "Parse error when printing AST: " ++ show err