module Main where

import qualified Data.Map as Map
import Maskinspraak8000.AST
import Maskinspraak8000.Interpreter

echo :: Prog
echo = Prog (Map.fromList [("echo", Abs []
                                        Map.empty
                                        [VarTerm "getLine",
                                         AbsTerm $ Abs ["s"]
                                                       Map.empty
                                                       [VarTerm "putStrLn",
                                                        VarTerm "s",
                                                        VarTerm "echo"]])])
            [VarTerm "echo"]

main = runProg echo globalEnv

