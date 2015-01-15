{-# LANGUAGE DeriveDataTypeable #-}
module M4 where

import Data.Typeable.Internal
import Text.ParserCombinators.Parsec
import Data.Maybe
import Control.Exception


data Expression = SExpr [Expression]
                | Number Integer
                | Sym String
                  deriving (Show)


data LispError = SyntaxError deriving (Show, Typeable)

instance Exception LispError


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


eval :: Expression -> IO (Integer)
eval (Number n) = return n
eval (SExpr e) = evalSexpr e
eval _ = throwIO SyntaxError


evalSexpr :: [Expression] -> IO (Integer)
evalSexpr ((Sym "+"):ex:ey:[]) = do
   x <- eval ex
   y <- eval ey
   return $ x + y
evalSexpr _ = throwIO SyntaxError


runEval :: Either ParseError Expression -> IO (Integer)
runEval (Right x) = eval x
runEval (Left x) = throwIO SyntaxError

main :: IO ()
main = do
  let ast = parseExpr "(+ 1 (+ 2 3))"
  putStr $ (show ast) ++ "\n"
  res <- runEval ast
  putStr $ (show res) ++ "\n"



