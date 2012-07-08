module Maskinspraak8000.Parser.Lean where

import Control.Monad (liftM2)
import qualified Data.Map as Map

import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

import qualified Maskinspraak8000.AST as AST

(<&>) :: Monad m => m a -> m b -> m (a, b)
(<&>) = liftM2 (,)

languageDef = haskellStyle
                { Token.reservedOpNames = ["<=="],
                  Token.reservedNames   = ["def"] }

lexer = Token.makeTokenParser languageDef

whiteSpace = Token.whiteSpace    lexer
identifier = Token.identifier    lexer
number     = Token.integer       lexer
string     = Token.stringLiteral lexer
brackets   = Token.brackets      lexer
braces     = Token.braces        lexer
reserved   = Token.reserved      lexer
reservedOp = Token.reservedOp    lexer
comma      = Token.comma         lexer

run :: Parser ()
run = reservedOp "<=="

def :: Parser ()
def = reserved "def"

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` comma

term :: Parser AST.Term
term =  fmap AST.NumTerm number
    <|> fmap AST.StrTerm string
    <|> fmap AST.VarTerm identifier
    <|> fmap AST.AbsTerm abstraction
    <?> "term"

abstraction :: Parser AST.Abs
abstraction =
    do formals     <- formalParams
       (defs, app) <- braces (definitions <&> application)
       return $ AST.Abs formals defs app
    <?> "abstraction"

formalParams :: Parser AST.Formals
formalParams = brackets $ commaSep identifier

definitions :: Parser AST.Defs
definitions = fmap Map.fromList $ many definition
    where
        definition :: Parser (AST.Id, AST.Abs)
        definition = def >> (identifier <&> abstraction)

application :: Parser AST.App
application =
    do fun  <- term
       _    <- run
       args <- commaSep term
       return $ fun : args
    <?> "application"

program :: Parser AST.Prog
program =
    do whiteSpace
       defs <- definitions
       app  <- application
       return $ AST.Prog defs app
    <?> "program"

library :: Parser AST.Lib
library = (whiteSpace >> fmap AST.Lib definitions)
          <?> "library"

programOrLibrary :: Parser (Either AST.Prog AST.Lib)
programOrLibrary =
    do whiteSpace
       defs   <- definitions
       optApp <- optionMaybe application
       return $ case optApp of
           Just app -> Left $ AST.Prog defs app
           Nothing  -> Right $ AST.Lib defs

