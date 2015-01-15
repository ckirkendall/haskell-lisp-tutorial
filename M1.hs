module M1 where

import Text.ParserCombinators.Parsec
import Data.Maybe


number :: Parser Integer
number = fmap read $ many1 $ oneOf "1234567890"


expression :: Parser Integer
expression = do
    char '('
    char '+'
    char ' '
    x <- number
    char ' '
    y <- number
    char ')'
    return $ x + y
              
parseExpr :: String -> Either ParseError Integer
parseExpr = parse expression "(unknown)"


main :: IO ()
main = do  
  let x = show $ parseExpr "(+ 1 (+ 2 3))"
  putStr $ x ++ "\n"


tmp :: String -> String
tmp x = x



