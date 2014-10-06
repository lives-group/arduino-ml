module Compiler.Core.Parser(parser) where

import Data.Functor((<$>))
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import Text.ParserCombinators.Parsec

import Compiler.Core.Syntax

----------------------------------
-- Simple parser for the core language
----------------------------------

-- parser

parser :: String -> Either String Term
parser s = either (Left . show) Right (parse term "" s)

var = Var <$> identifier

abst
  = do
     reservedOp "\\"
     v <- identifier
     dot
     t <- term
     return (Lam v t)

nonApp = var <|> abst <|> letp <|> Const <$> litp <|> parens term

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


term = chainl1 nonApp (return App)

-- lexer

charLiteral = P.charLiteral lexer
float = P.float lexer
integer = fromInteger <$> P.integer lexer
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
