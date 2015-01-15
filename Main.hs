{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Data.Typeable.Internal
import Text.ParserCombinators.Parsec
import Data.Map
import Data.Maybe
import Control.Exception

data LispError = SyntaxError String deriving (Show, Typeable)

instance Exception LispError

data Expression = SExpr [Expression]
                | Identifier String
                | Number Integer
                  deriving (Show)


program :: Parser [Expression]
program =
  do first <- expression
     next <- remainingExpressions
     return (first : next)

remainingExpressions :: Parser [Expression]
remainingExpressions =
    (oneOf " ,\n" >> program)
    <|> return []

identifier :: Parser Expression
identifier = fmap Identifier $ many1 $ oneOf "&abcdefghijklmnopqrstuvwxyz_-+"

number :: Parser Expression
number = fmap (Number . read) $ many1 $ oneOf "1234567890"

sexp :: Parser Expression
sexp = do
    char '('
    exp <- program
    char ')'
    return $ SExpr exp


expression :: Parser Expression
expression = sexp 
    <|> number 
    <|> identifier
    

parseLisp :: String -> Either ParseError [Expression]
parseLisp = parse program "(unknown)"




eval :: Expression -> IO(Integer)
eval (Number n) = return n
eval (Identifier s) = do throwIO $ SyntaxError "invalid symbol placement"
eval (SExpr ((Identifier "+"):(Number n):rv)) = do
  l <- eval $ SExpr $ (Identifier "+"):rv
  return (n + l)
eval (SExpr ((Identifier "+"):[])) = return 0
eval (SExpr _) = do throwIO $ SyntaxError "invalid sexpr" 



main :: IO ()
main = do
  res <- runEval $ parseLisp "(+ 1 2)"
  putStr $ show res


runEval :: Either ParseError [Expression] -> IO(Integer)
runEval (Left error) = throwIO(SyntaxError (show error))
runEval (Right (h:t)) = eval h


