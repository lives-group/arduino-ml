module Compiler.Core.Parser(parser) where

import Prelude hiding (EQ,LT)

import Data.Functor((<$>))
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.ParserCombinators.Parsec
import Text.Parsec.Expr

import Compiler.Core.Syntax
import Compiler.Utils.Monad

----------------------------------
-- Simple parser for the core language
----------------------------------

-- parser

parser :: String -> PhaseM Term
parser s = either (throwError . show) return (parse term "" s)

var = Var <$> identifier

abst
  = do
     reservedOp "\\"
     v <- identifier
     dot
     t <- term
     return (Lam v t)

nonApp = var <|> abst <|> letp <|> Const <$> litp <|> parens term <|> ifp

litp = ILit <$> integer     <|>
       CLit <$> charLiteral <|>
       DLit <$> float

letp = do
         reserved "let"
         (i,t) <- braces $ do {
                     i <- identifier ;
                     reservedOp "="  ;
                     t <- term       ;
                     return (i,t)
                  }
         reserved "in"
         t' <- term
         return (Let i t t')

ifp = do
         reserved "if"
         t <- term
         reserved "then"
         t1 <- term
         reserved "else"
         t2 <- term
         return (If t t1 t2)

term' = chainl1 nonApp (return App)

term = buildExpressionParser table term'
       where
         table = [[ Infix (reservedOp "*" >> return (Op MULT)) AssocLeft],
                  [ Infix (reservedOp "+" >> return (Op ADD)) AssocLeft,
                    Infix (reservedOp "-" >> return (Op MINUS)) AssocLeft],
                  [ Infix (reservedOp "<" >> return (Op LT)) AssocNone,
                    Infix (reservedOp "==" >> return (Op EQ)) AssocNone]]

-- lexer

charLiteral = P.charLiteral lexer
float = P.float lexer
integer = fromInteger <$> P.natural lexer
identifier = P.identifier lexer
dot = P.dot lexer
semi = P.semi lexer
braces = P.braces lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer
parens = P.parens lexer
spaces = P.whiteSpace lexer

lexer :: P.TokenParser st
lexer = P.makeTokenParser miniML

miniML :: LanguageDef st
miniML = haskellDef
