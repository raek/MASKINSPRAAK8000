module Main where

import qualified Data.Map as Map
import Maskinspraak8000.AST
import Maskinspraak8000.Interpreter

echo :: Abs
echo = Abs []
           (Map.fromList [("echo", Abs []
                                       Map.empty
                                       [VarTerm "getLine",
                                        AbsTerm $ Abs ["s"]
                                                      Map.empty
                                                      [VarTerm "putStrLn",
                                                       VarTerm "s",
                                                       VarTerm "echo"]])])
           [VarTerm "echo"]

main = runAbs echo [] globalEnv

