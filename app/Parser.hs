module Parser where
import Text.Parsec
import Type (Ast (..))
import Data.Functor.Identity ( Identity )
import Debug.Trace (trace)

var :: ParsecT String u Identity [Char]
var = do
  x <- many1 alphaNum
  return x

varTerm :: ParsecT String u Identity Ast
varTerm = Var <$> var

fn :: ParsecT String u Identity Ast
fn = do
  char '\\'
  spaces
  args <- argList
  spaces
  char '.'
  spaces
  t <- term
  return $ foldr Lam t args
  where
    argList = do
      h <- var
      (h:) <$> ((do
        skipMany1 space
        argList) <|> return [])


app :: ParsecT String u Identity Ast
app = do
  l <- termLevel0List
  if length l == 1
    then return $ head l
    else return $ foldl1 App l
  where
    termLevel0List = do
      h <- termLevel0
      t <- ((do
        skipMany1 space
        tmp <- getInput
        termLevel0List) <|> return [])
      return $ h:t

paren :: ParsecT String u Identity Ast
paren = do
  char '('
  spaces
  t <- term
  spaces
  char ')'
  return t

termLevel0 :: ParsecT String u Identity Ast
termLevel0 = do
  paren <|> fn <|> varTerm

term :: ParsecT String u Identity Ast
term = app

parseL :: String -> Either ParseError Ast
parseL = parse term ""

