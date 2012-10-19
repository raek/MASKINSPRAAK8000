module Maskinspraak8000.Console where

import Prelude hiding (getLine, putStrLn)
import qualified System.IO

data ConsoleAction a = Stop a
                     | GetLine (String -> ConsoleAction a)
                     | PutStrLn String (ConsoleAction a)

getLine :: ConsoleAction String
getLine = GetLine Stop

putStrLn :: String -> ConsoleAction ()
putStrLn s = PutStrLn s (Stop ())

instance Monad ConsoleAction where
  return = Stop
  (Stop x)       >>= f = f x
  (GetLine k)    >>= f = GetLine (\s -> k s >>= f)
  (PutStrLn s k) >>= f = PutStrLn s (k >>= f)

instance Functor ConsoleAction where
  fmap g m = m >>= (return . g)

runConsole :: ConsoleAction a -> IO a
runConsole (Stop x)       = return x
runConsole (GetLine k)    = System.IO.getLine >>= (runConsole . k)
runConsole (PutStrLn s k) = System.IO.putStrLn s >> (runConsole k)