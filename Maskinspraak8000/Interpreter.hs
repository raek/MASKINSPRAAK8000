module Maskinspraak8000.Interpreter where

import Maskinspraak8000.AST
import qualified Data.Map as Map

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

type ExecOutcome = IO (Maybe [Val])

execStep :: [Val] -> ExecOutcome
execStep (SpecFun id:vals)      = execSpecFun id vals
execStep (CompFun env abs:vals) = return $ Just $ execCompFun env abs vals

execSpecFun :: SpecId -> [Val] -> ExecOutcome
execSpecFun "exit"  []         = return Nothing
execSpecFun "error" [StrVal s] = return $ error s
execSpecFun id      vals       = fmap Just $ execSpecFun' id vals

execSpecFun' :: SpecId -> [Val] -> IO [Val]
execSpecFun' "get_line"      [k]           = fmap (\l -> [k, StrVal l]) getLine
execSpecFun' "put_str_ln"    [StrVal s, k] = putStrLn s >> return [k]
execSpecFun' "string_to_num" [StrVal s, k] = return [k, NumVal $ read s]
execSpecFun' "num_to_string" [NumVal n, k] = return [k, StrVal $ show n]

globalEnv :: Env
globalEnv = insertVals [(id, SpecFun id) | id <- ids] Map.empty
    where ids = ["exit", "error", "get_line", "put_str_ln", "string_to_num", "num_to_string"]

execCompFun :: Env -> Abs -> [Val] -> [Val]
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
eval env (VarTerm id)  = let (Just x) = Map.lookup id env
                         in x
eval env (AbsTerm abs) = CompFun env abs

absVal :: Abs -> Val
absVal abs = CompFun undefined abs

execAll :: [Val] -> IO ()
execAll vals =
    do outcome <- execStep vals
       case outcome of
           Nothing    -> return ()
           Just vals' -> execAll vals'

runApp :: App -> Env -> IO ()
runApp terms env = execAll $ map (eval env) terms

runAbs :: Abs -> [Val] -> Env -> IO ()
runAbs abs vals env = execAll $ (CompFun env abs):vals

runProg :: Prog -> Env -> IO ()
runProg (Prog defs app) = runAbs (Abs [] defs app) []

