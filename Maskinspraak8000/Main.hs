module Main where

import Maskinspraak8000.AST (Prog)
import Maskinspraak8000.Interpreter (runProg, globalEnv)
import Maskinspraak8000.Parser.Lean (program)
import Text.Parsec.Prim (parse)
import System.Environment (getArgs)

type Error = String

syntax :: Error
syntax = "Syntax: M8000 program.M8s"

filePathFromArgs :: [String] -> Either Error FilePath
filePathFromArgs args = case args of
    [filePath] -> Right filePath
    _          -> Left syntax

parseProgram :: String -> String -> Either Error Prog
parseProgram filePath fileContent =
    case parse program filePath fileContent of
        Left error -> Left $ show error
        Right prog -> Right prog

main = do args <- getArgs
          case filePathFromArgs args of
              Left error     -> putStrLn error
              Right filePath -> do
                  fileContent <- readFile filePath
                  case parseProgram filePath fileContent of
                      Left error -> putStrLn error
                      Right prog -> runProg prog globalEnv

