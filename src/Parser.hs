
module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Data.Map as Map

import Ast

-- TODO: a parametric parser, parse Term a

type Env a = Map.Map String a

iden :: Parser String
iden = many1 (alphaNum <|> oneOf "_-+=!?,<>*/#@$%^&")

var :: Env a -> Parser (Term a)
var env = do
  i <- iden
  maybe (parserFail $ "unbound variable " ++ i) (return . Var) $ Map.lookup i env

lam :: Env a -> Parser (Ast.Term a)
lam env = do
  _ <- oneOf "λ\\"
  spaces
  ids <- (iden `endBy1` spaces)
  _ <- oneOf ".·"
  buildBodyParser env ids
  where buildBodyParser :: Env a -> [ String ] -> Parser (Ast.Term a)
        buildBodyParser env [] = term env
        buildBodyParser env (h : t) =
          fmap Lam $ buildBodyParser (Map.insert h Nothing $ fmap Just env) t

term :: Env a -> Parser (Ast.Term a)
term env = do
  spaces
  let atom = lam env <|> var env <|> between (char '(') (char ')') (term env)
  atom `chainl1` (many1 space >> return App)

parser :: Env a -> String -> Either ParseError (Term a)
parser env str = parse oneExpr "(source)" str
  where oneExpr = do
          it <- term env -- for tests
          eof
          return it
