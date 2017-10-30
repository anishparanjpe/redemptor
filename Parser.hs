module Parser where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as P

import Language

lexer :: P.TokenParser ()
lexer = P.makeTokenParser haskellDef

identifier :: Parser String
identifier = P.identifier lexer

integer :: Parser Integer
integer = P.integer lexer

symbol :: String -> Parser String
symbol = P.symbol lexer

pProgram :: Parser Program
pProgram = pSc `sepBy1` (symbol ";")

pSc :: Parser ScDefn
pSc = do
  (name : args) <- many1 identifier
  _ <- symbol "="
  expr <- pExpr
  return (name, args, expr)

pExpr :: Parser Expr
pExpr = chainl1 pAExpr $ optional space  >> return EAp

pAExpr :: Parser Expr
pAExpr = EVar <$> identifier
     <|> ENum <$> integer
     <|> between (symbol "(") (symbol ")") pExpr

parseProgram :: String -> Either ParseError Program
parseProgram = parse pProgram ""
