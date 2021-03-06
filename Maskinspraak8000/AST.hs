module Maskinspraak8000.AST where

import Data.Map(Map)

type Id = String

data Term = NumTerm Integer
          | StrTerm String
          | VarTerm Id
          | AbsTerm Abs

type Formals = [Id]

type Defs = Map Id Abs

type App = [Term]

data Abs = Abs { formals :: Formals,
                 defs    :: Defs,
                 app     :: App }

data Prog = Prog Defs App

data Lib = Lib Defs

