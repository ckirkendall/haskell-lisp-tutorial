module M3 where

import Text.ParserCombinators.Parsec
import Data.Maybe

data Expression = SExpr [Expression]
                | Number Integer
                | Sym String
                  deriving (Show)
                  

number :: Parser Expression
number = fmap (Number . read) $ many1 $ oneOf "1234567890"


symbol :: Parser Expression
symbol = fmap Sym $ many1 $ oneOf "+"


sexp :: Parser Expression
sexp = do
    char '('
    s <- symbol
    char ' '
    x <- expression
    char ' '
    y <- expression
    char ')'
    return $ SExpr (s:x:[y])


expression :: Parser Expression
expression = sexp 
    <|> number


parseExpr :: String -> Either ParseError Expression
parseExpr = parse expression "(unknown)"


main :: IO ()
main = do
  let x = show $ parseExpr "(+ 1 (+ 2 3))"
  putStr $ x ++ "\n"



