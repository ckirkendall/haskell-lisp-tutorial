module M2 where

import Text.ParserCombinators.Parsec
import Data.Maybe


number :: Parser Integer
number = fmap read $ many1 $ oneOf "1234567890"


sexp :: Parser Integer
sexp = do
    char '('
    char '+'
    char ' '
    x <- expression
    char ' '
    y <- expression
    char ')'
    return $ x + y


expression :: Parser Integer
expression = sexp 
    <|> number


parseExpr :: String -> Either ParseError Integer
parseExpr = parse expression "(unknown)"


main :: IO ()
main = do
  let x = show $ parseExpr "(+ 1 (+ 2 3))"
  putStr $ x ++ "\n"

