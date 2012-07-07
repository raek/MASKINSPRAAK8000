module Maskinspraak8000.AST where

import Data.Map(Map)

type Id = String

data Term = NumTerm Integer
          | StrTerm String
          | VarTerm Id
          | AbsTerm Abs

data Abs = Abs { formals :: [Id],
                 defs    :: Map Id Abs,
                 app     :: [Term] }

