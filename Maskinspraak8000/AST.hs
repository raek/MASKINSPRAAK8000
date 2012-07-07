module Maskinspraak8000.AST where

import Data.Map(Map)

type Id = String

data Abs = Abs { formals :: [Id],
                 defs    :: Map Id Abs,
                 app     :: [Term] }

data Term = LitTerm Lit
          | VarTerm Id
          | AbsTerm Abs

data Lit = NumLit Integer
         | StrLit String

