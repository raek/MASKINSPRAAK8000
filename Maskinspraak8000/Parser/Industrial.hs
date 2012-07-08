module Maskinspraak8000.Parser.Industrial
    (
      program
    , library
    , programOrLibrary
    )
where

import Text.Parsec (oneOf, sepBy)
import Text.Parsec.Prim
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (option, optionMaybe)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

import Data.Functor
import qualified Data.Map as Map

import qualified Maskinspraak8000.AST as AST

-- lexer
smallAlphaNum = oneOf ("åäö_" ++ ['a'..'z'] ++ ['0'..'9'])

langDef = emptyDef
           { Token.commentLine   = "KOMMENTAR"
           , Token.identStart    = smallAlphaNum
           , Token.identLetter   = smallAlphaNum
           , Token.reservedNames = [ "MASKIN"
                                   , "PÅBÖRJA"
                                   , "AVSLUTA"
                                   , "KÖR"
                                   , "MED"
                                   , "OCH"
                                   , "LÅT"
                                   , "VARA"
                                   ]
           , Token.caseSensitive = True
           }

lexer = Token.makeTokenParser langDef

identifier = Token.identifier    lexer
number     = Token.integer       lexer
string     = Token.stringLiteral lexer
keyword    = Token.reserved      lexer
brackets   = Token.brackets      lexer
semiColon  = Token.symbol        lexer ";"
whiteSpace = Token.whiteSpace    lexer


-- parser
abstraction :: Parser AST.Abs
abstraction =
    do keyword "MASKIN"
       formals <- brackets (identifier `sepBy` semiColon)
       keyword "PÅBÖRJA"
       defs <- definitions
       app <- application
       keyword "AVSLUTA"
       return (AST.Abs formals defs app)

definition :: Parser (AST.Id, AST.Abs)
definition =
    do keyword "LÅT"
       name <- identifier
       keyword "VARA"
       abs <- abstraction
       return (name, abs)

definitions :: Parser AST.Defs
definitions =
    do defs <- many definition
       return (Map.fromList defs)

application :: Parser [AST.Term]
application =
    do keyword "KÖR"
       run <- runnable
       args <- option [] actuals
       return (run : args)

actuals :: Parser [AST.Term]
actuals =
    do keyword "MED"
       term `sepBy` (keyword "OCH")

runnable :: Parser AST.Term
runnable = AST.VarTerm <$> identifier
       <|> AST.AbsTerm <$> abstraction

term :: Parser AST.Term
term = AST.NumTerm <$> number
   <|> AST.StrTerm <$> string
   <|> AST.VarTerm <$> identifier
   <|> AST.AbsTerm <$> abstraction


-- exported
program :: Parser AST.Prog
program =
    do whiteSpace
       defs <- definitions
       app <- application
       return (AST.Prog defs app)

library :: Parser AST.Lib
library =
    do whiteSpace
       defs <- definitions
       return (AST.Lib defs)

programOrLibrary :: Parser (Either AST.Prog AST.Lib)
programOrLibrary =
    do whiteSpace
       defs <- definitions
       app <- optionMaybe application
       case app of
           Just a  -> return $ Left (AST.Prog defs a)
           Nothing -> return $ Right (AST.Lib defs)
