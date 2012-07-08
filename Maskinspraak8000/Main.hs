module Main where

import Maskinspraak8000.AST
import Maskinspraak8000.Interpreter
import qualified Maskinspraak8000.Parser.Lean as Parser
import Text.Parsec.String

main = do echoRes <- parseFromFile Parser.program "examples/echo.M8s"
          case echoRes of
              Left error -> print error
              Right echo -> runProg echo globalEnv

