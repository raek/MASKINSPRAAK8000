module Maskinspraak8000.Interpreter where

import Prelude hiding (getLine, putStrLn)
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Maskinspraak8000.AST
import Maskinspraak8000.Console

type SpecId = String

data Val = NumVal Integer
         | StrVal String
         | CompFun Env Abs
         | SpecFun SpecId

type Env = Map.Map Id Val

insertVal :: Id -> Val -> Env -> Env
insertVal id val e = e''
    where e'  = Map.insert id val e
          e'' = fmap (updateVal e'') e'

insertVals :: [(Id, Val)] -> Env -> Env
insertVals [] env            = env
insertVals ((id,val):xs) env = insertVals xs (insertVal id val env)

updateVal :: Env -> Val -> Val
updateVal e (CompFun _ abs) = CompFun e abs
updateVal _ x = x

type Call        = [Val]
type ExecOutcome = ConsoleAction (Maybe Call)

execStep :: Call -> ExecOutcome
execStep (SpecFun id:vals)      = execSpecFun id vals
execStep (CompFun env abs:vals) = return $ Just $ execCompFun env abs vals

execSpecFun :: SpecId -> Call -> ExecOutcome
execSpecFun "exit"  []         = return Nothing
execSpecFun "error" [StrVal s] = return $ error s
execSpecFun id      vals       = fmap Just $ execSpecFun' id vals

execSpecFun' :: SpecId -> Call -> ConsoleAction Call
execSpecFun' "get_line"      [k]           = fmap (\l -> [k, StrVal l]) getLine
execSpecFun' "put_str_ln"    [StrVal s, k] = putStrLn s >> return [k]
execSpecFun' "string_to_num" [StrVal s, k] = return [k, NumVal $ read s]
execSpecFun' "num_to_string" [NumVal n, k] = return [k, StrVal $ show n]
execSpecFun' "eq"            [NumVal x, NumVal y, t, f] = return [if x == y then t else f]
execSpecFun' "eq"            [StrVal x, StrVal y, t, f] = return [if x == y then t else f]
execSpecFun' "mul"           [NumVal x, NumVal y, k] = return [k, NumVal (x * y)]
execSpecFun' "sub"           [NumVal x, NumVal y, k] = return [k, NumVal (x - y)]

globalEnv :: Env
globalEnv = insertVals [(id, SpecFun id) | id <- ids] Map.empty
    where ids = ["exit", "error", "get_line", "put_str_ln", "string_to_num", "num_to_string", "eq", "mul", "sub"]

execCompFun :: Env -> Abs -> Call -> Call
execCompFun env abs vals = vals'
    where
        argPairs = zip (formals abs) vals
        env'     = insertVals argPairs env
        defPairs = fmap (fmap absVal) $ Map.assocs $ defs abs
        env''    = insertVals defPairs env'
        vals'    = map (eval env'') $ app abs

eval :: Env -> Term -> Val
eval env (NumTerm n)   = NumVal n
eval env (StrTerm s)   = StrVal s
eval env (VarTerm id)  = fromJust $ Map.lookup id env
eval env (AbsTerm abs) = CompFun env abs

absVal :: Abs -> Val
absVal abs = CompFun undefined abs

execAll :: Call -> ConsoleAction ()
execAll vals =
    do outcome <- execStep vals
       case outcome of
           Nothing    -> return ()
           Just vals' -> execAll vals'

runApp :: App -> Env -> ConsoleAction ()
runApp terms env = execAll $ map (eval env) terms

runAbs :: Abs -> Call -> Env -> ConsoleAction ()
runAbs abs vals env = execAll $ (CompFun env abs):vals

runProg :: Prog -> Env -> ConsoleAction ()
runProg (Prog defs app) = runAbs (Abs [] defs app) []

